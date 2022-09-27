package app.services

import app.Pickup
import app.PickupInfo
import app.Status
import app.Train
import zio.clock.Clock

import java.util.UUID
import zio.Has
import zio.Schedule
import zio.UIO
import zio.URIO
import zio.URLayer
import zio.ZIO
import zio.stm.STM

import java.time.Duration

trait RailwayDispatcher[F[_]] {
  def registerTrains(quantity: Int): F[Int]
  def moveTrains: F[Unit]
  def getTrains: F[Vector[Train]]
  def enqueuePickup(request: Pickup): F[Unit]
  def dispatchPickups(processingPickupsNumber: Int): F[Unit]
  def getPickupStatus(requestId: UUID): F[PickupInfo]
}

object RailwayDispatcher {
  def dispatch(numberOfDispatchingRequests: Int,
               trainsSchedule: Duration,
               pickupsSchedule: Duration): URIO[Has[RailwayDispatcher[UIO]] with Clock, Unit] = {
    moveTrains
      .repeat(Schedule.spaced(trainsSchedule))
      .zipPar(dispatchPickups(numberOfDispatchingRequests).repeat(Schedule.spaced(pickupsSchedule)))
      .unit
  }

  def registerTrains(quantity: Int): URIO[Has[RailwayDispatcher[UIO]], Int] =
    ZIO.serviceWith[RailwayDispatcher[UIO]](_.registerTrains(quantity))

  def moveTrains: URIO[Has[RailwayDispatcher[UIO]], Unit] =
    ZIO.serviceWith[RailwayDispatcher[UIO]](_.moveTrains)

  def getTrains: URIO[Has[RailwayDispatcher[UIO]], Vector[Train]] =
    ZIO.serviceWith[RailwayDispatcher[UIO]](_.getTrains)

  def getPickupStatus(requestId: UUID): URIO[Has[RailwayDispatcher[UIO]], PickupInfo] =
    ZIO.serviceWith[RailwayDispatcher[UIO]](_.getPickupStatus(requestId))

  def enqueuePickup(request: Pickup): URIO[Has[RailwayDispatcher[UIO]], Unit] =
    ZIO.serviceWith[RailwayDispatcher[UIO]](_.enqueuePickup(request))

  def dispatchPickups(processingPickupsNumber: Int): URIO[Has[RailwayDispatcher[UIO]], Unit] =
    ZIO.serviceWith[RailwayDispatcher[UIO]](_.dispatchPickups(processingPickupsNumber))
}

final case class RailwayDispatcherLive(
    trainService: TrainsService[UIO],
    pickupService: PickupService[UIO]
) extends RailwayDispatcher[UIO] {

  override def registerTrains(quantity: Int): UIO[Int] =
    trainService.addTrains(quantity)

  override def enqueuePickup(request: Pickup): UIO[Unit] = {
    pickupService.enqueue(request)
  }

  override def moveTrains: UIO[Unit] =
    trainService.moveTrains

  override def dispatchPickups(processingPickupsNumber: Int): UIO[Unit] = {
    for {
      requests <- pickupService.dequeueUpTo(processingPickupsNumber)
      fibers <- ZIO.foreach(requests) { req =>
        trainService
          .updateState(req)
          .flatMap { trainOpt =>
            trainOpt.fold(reenqueue(req)) { train =>
              pickupService
                .updatePickupStatus(req.id, Status.PickedUp(train.trainId, train.getDistanceTo(req)))
                .unit
            }
          }
          .fork
      }
      _ <- ZIO.foreach(fibers)(_.join)
    } yield ()
  }

  private def reenqueue(req: Pickup) = {
    pickupService
      .updatePickupStatus(req.id, Status.Waiting)
      .flatMap(_ => pickupService.enqueue(req))
  }

  override def getPickupStatus(requestId: UUID): UIO[PickupInfo] =
    pickupService.getPickupStatus(requestId)

  override def getTrains: UIO[Vector[Train]] = trainService.getTrains
}

object RailwayDispatcherLive {
  val layer: URLayer[Has[TrainsService[UIO]] with Has[PickupService[UIO]], Has[RailwayDispatcher[UIO]]] =
    (RailwayDispatcherLive(_, _)).toLayer
}
