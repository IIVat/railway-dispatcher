package app.services

import app.Pickup
import app.Train
import zio.*
import zio.stm.STM
import zio.stm.TRef

import java.util.concurrent.atomic.AtomicReference

trait TrainsService[F[_]] {
  def addTrains(quantity: Int): F[Int]
  def moveTrains: F[Unit]
  def getTrains: F[Vector[Train]]
  def updateState(request: Pickup): F[Option[Train]]
}

object TrainsService {
  def addTrains(quantity: Int): URIO[Has[TrainsService[UIO]], Int] =
    ZIO.serviceWith[TrainsService[UIO]](_.addTrains(quantity))

  def moveTrains: URIO[Has[TrainsService[UIO]], Unit] =
    ZIO.serviceWith[TrainsService[UIO]](_.moveTrains)

  def getTrains: URIO[Has[TrainsService[UIO]], Vector[Train]] =
    ZIO.serviceWith[TrainsService[UIO]](_.getTrains)

  def updateState(request: Pickup): URIO[Has[TrainsService[UIO]], Option[Train]] =
    ZIO.serviceWith[TrainsService[UIO]](_.updateState(request))
}

final case class TrainsServiceLive(trainsRef: TRef[Vector[Train]]) extends TrainsService[UIO] {

  override def getTrains: UIO[Vector[Train]] = {
    (for {
      value <- trainsRef.get
    } yield value).commit
  }

  override def updateState(request: Pickup): UIO[Option[Train]] = {
    (for {
      trainOpt <- trainsRef.modify { state =>
        val optimalTrain = state
          .filter(_.isAlongTheWay(request))
          .minByOption(_.getDistanceTo(request))
        optimalTrain.fold((Option.empty[Train], state)) { train =>
          val newTrain = train.pickup(request)
          (Some(newTrain), state.updated(train.trainId, newTrain))
        }
      }
    } yield trainOpt).commit
  }

  override def addTrains(quantity: Int): UIO[Int] = {

    (for {
      trains <- STM.foreach(Vector.tabulate(quantity)(Train.init).flatten)(x => STM.succeed(x))
      size <- trainsRef
        .updateAndGet { state =>
          state ++ trains
        }
        .map(_.size)

    } yield size).commit
  }

  override def moveTrains: UIO[Unit] =
    trainsRef.updateAndGet(_.map(_.move)).commit.unit

}

object TrainsServiceLive {
  val layer: URLayer[Has[TRef[Vector[Train]]], Has[TrainsService[UIO]]] = (TrainsServiceLive(_)).toLayer
}
