import io.circe.*

import java.time.Instant
import java.util.UUID

package object app {
  final case class TerminalId(value: Int) extends AnyVal
  final case class Terminal(terminalId: TerminalId, usersIn: Int, usersOut: Int)

  final case class Train(
      trainId: Int,
      currentLocation: Terminal = Terminal(TerminalId(0), 0, 0),
      stops: Vector[Terminal] = Vector.empty,
      capacity: Int = 0
  ) {
    def move: Train = {
      if (capacity == 0 && stops.isEmpty) {
        this
      } else {

        val stop =
          if (isNotMoving) currentLocation
          else if (isMovingRight || isMovingRightAndThenLeft) currentStop(currentLocation.terminalId.value + 1)
          else if (isMovingLeft || isMovingLeftAndThenRight) currentStop(currentLocation.terminalId.value - 1)
          else currentLocation

        println(s"isMRTL: ${isMovingRightAndThenLeft}")
        println(s"isMR: ${isMovingRight}")
        println(s"isMLTR: ${isMovingLeftAndThenRight}")
        println(s"isML: ${isMovingLeft}")
        println("stop:  " + stop + s" capacity: $capacity")

        copy(currentLocation = stop, stops = updateStops(stop), capacity = updateCapacity(stop))
      }
    }

    def isAlongTheWay(user: Pickup): Boolean = {
      stops.isEmpty ||
      (isMovingRight && user.isMovingRight) ||
      (isMovingLeft && user.isMovingLeft) ||
      (isMovingRightAndThenLeft &&
      user.currentLocation.value >= currentLocation.terminalId.value &&
      user.destination.value <= currentLocation.terminalId.value) ||
      (isMovingLeftAndThenRight &&
      user.currentLocation.value <= currentLocation.terminalId.value &&
      user.destination.value >= currentLocation.terminalId.value)
    }

    def getDistanceTo(user: Pickup): Int =
      math.abs(currentLocation.terminalId.value - user.currentLocation.value)

    def pickup(user: Pickup): Train = {
      val changeable =
        if (isMovingLeftAndThenRight || isMovingRightAndThenLeft && stops.size > 2) stops.drop(2)
        else stops

      val pickupPoint = changeable
        .find(_.terminalId == user.currentLocation)
        .map(t => t.copy(usersIn = t.usersIn + 1))
        .getOrElse(Terminal(user.currentLocation, usersIn = 1, usersOut = 0))

      val destinationPoint = changeable
        .find(_.terminalId == user.destination)
        .map(t => t.copy(usersOut = t.usersOut + 1))
        .getOrElse(Terminal(user.destination, usersIn = 0, usersOut = 1))

      copy(
        stops =
          if (stops.isEmpty) Vector(pickupPoint, destinationPoint)
          else
            stops.map {
              case t if t.terminalId == user.currentLocation => pickupPoint
              case t if t.terminalId == user.destination     => destinationPoint
              case t                                         => t
            }
      )
    }

    private def isNotMoving = isMovingLeft && isMovingRight

    private def isMovingRight: Boolean =
      !isMovingRightAndThenLeft && (capacity != 0 || stops.nonEmpty) &&
        stops.forall(_.terminalId.value >= currentLocation.terminalId.value)

    private def isMovingLeft: Boolean =
      !isMovingLeftAndThenRight && (capacity != 0 || stops.nonEmpty) &&
        stops.forall(_.terminalId.value <= currentLocation.terminalId.value)

    private def isMovingRightAndThenLeft: Boolean = {
      (stops.toList match {
        case pickup :: destination :: _
            if currentLocation.terminalId.value < pickup.terminalId.value &&
              pickup.terminalId.value >= destination.terminalId.value =>
          true
        case _ => false
      })
    }

    private def isMovingLeftAndThenRight: Boolean = {
      (stops.toList match {
        case pickup :: destination :: _
            if currentLocation.terminalId.value > pickup.terminalId.value &&
              pickup.terminalId.value <= destination.terminalId.value =>
          true
        case _ => false
      })
    }

    private def updateStops(stop: Terminal): Vector[Terminal] = {
      val (constant, changeable) = {
        if (isNotMoving) (Vector.empty, Vector.empty)
        else if ((isMovingRightAndThenLeft || isMovingRightAndThenLeft) &&
                 stops.head.terminalId.value != currentLocation.terminalId.value + 1) stops.splitAt(2)
        else (Vector.empty, stops)
      }
      val isRequiredStop = changeable.exists(_.terminalId == stop.terminalId)

      val changed = if (isRequiredStop) changeable.filterNot(_.terminalId == stop.terminalId) else changeable

      constant ++ changed
    }

    private def updateCapacity(stop: Terminal): Int =
      if (isNotMoving) 0 else capacity + stop.usersIn - stop.usersOut

    private def currentStop(terminalId: Int): Terminal = {
      val curStops =
        if ((isMovingRightAndThenLeft || isMovingRightAndThenLeft) &&
            stops.head.terminalId.value != currentLocation.terminalId.value + 1) stops.drop(2).toSet
        else stops.toSet

      curStops
        .find(_.terminalId.value == terminalId)
        .getOrElse(currentLocation.copy(terminalId = TerminalId(terminalId), usersIn = 0, usersOut = 0))
    }
  }

  object Train {
    def init(trainId: Int): Option[Train] = {
      if (trainId < 0 || trainId > 31) None
      else Some(Train(trainId))
    }
  }

  final case class PickupRequest(currentLocation: Int, destination: Int)

  final case class Pickup private (id: UUID, currentLocation: TerminalId, destination: TerminalId) {
    def isMovingRight: Boolean = currentLocation.value < destination.value
    def isMovingLeft: Boolean  = currentLocation.value > destination.value
  }

  object Pickup {
    def validate(id: UUID, currentLocation: Int, destination: Int): Either[String, Pickup] = {
      if (currentLocation < 0 || currentLocation > 31) Left("Invalid current location")
      else if (destination < 0 || destination > 31) Left("Invalid destination")
      else Right(Pickup(id, TerminalId(currentLocation), TerminalId(destination)))
    }
  }

  sealed abstract class Status(val value: String, val createdAt: Instant = Instant.now())

  object Status {
    final case object Queued  extends Status("queued")
    final case object Waiting extends Status("waiting")
    //assume that lineId == trainId
    final case class PickedUp(lineId: Int, waitingInterval: Int) extends Status("picked-up")

    implicit val encodeMode: Encoder[Status] = Encoder[String].contramap {
      case Queued =>
        Json
          .obj(
            Queued.value -> Json.obj(
              "createdAt" -> Json.fromString(Queued.createdAt.toString)
            )
          )
          .noSpaces
      case Waiting =>
        Json
          .obj(
            Waiting.value -> Json.obj(
              "createdAt" -> Json.fromString(Waiting.createdAt.toString)
            )
          )
          .noSpaces
      case pickedup @ PickedUp(trainId, waitingInterval) =>
        Json
          .obj(
            pickedup.value ->
              Json.obj(
                "lineId"          -> Json.fromInt(trainId),
                "waitingInterval" -> Json.fromInt(waitingInterval),
                "createdAt"       -> Json.fromString(pickedup.createdAt.toString)
              )
          )
          .noSpaces
    }
  }

  final case class PickupInfo(requestId: UUID, statuses: Vector[Status])
}
