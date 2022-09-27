package app.services

import app.Pickup
import app.PickupInfo
import app.PickupRequest
import app.Status
import zio.stm.STM
import zio.stm.TQueue
import zio.stm.TRef
import zio.stm.ZSTM
import zio.Has
import zio.UIO
import zio.URIO
import zio.URLayer
import zio.ZIO

import java.time.Instant
import java.util.UUID

trait PickupService[F[_]] {
  def enqueue(request: Pickup): F[Unit]
  def dequeueUpTo(max: Int): F[List[Pickup]]
  def getPickupStatus(requestId: UUID): F[PickupInfo]
  def updatePickupStatus(requestId: UUID, status: Status): F[PickupInfo]
}

object PickupService {
  def enqueue(request: Pickup): URIO[Has[PickupService[UIO]], Unit] =
    ZIO.serviceWith[PickupService[UIO]](_.enqueue(request))

  def dequeueUpTo(max: Int): URIO[Has[PickupService[UIO]], List[Pickup]] =
    ZIO.serviceWith[PickupService[UIO]](_.dequeueUpTo(max))

  def getPickupStatus(requestId: UUID): URIO[Has[PickupService[UIO]], PickupInfo] =
    ZIO.serviceWith[PickupService[UIO]](_.getPickupStatus(requestId))

  def updatePickupStatus(requestId: UUID, status: Status): URIO[Has[PickupService[UIO]], PickupInfo] =
    ZIO.serviceWith[PickupService[UIO]](_.updatePickupStatus(requestId, status))
}

final case class PickupServiceLive(requestsQueue: TQueue[Pickup], pickupInfoStorage: TRef[Map[UUID, Vector[Status]]])
    extends PickupService[UIO] {

  override def enqueue(request: Pickup): UIO[Unit] = {
    STM.atomically {
      for {
        _ <- requestsQueue.offer(request)
        _ <- updateStatus(request.id, Status.Queued)
      } yield ()
    }
  }

  override def dequeueUpTo(max: Int): UIO[List[Pickup]] = {
    (for {
      elems <- requestsQueue.takeUpTo(max)
    } yield elems).commit
  }

  override def getPickupStatus(requestId: UUID): UIO[PickupInfo] = {
    pickupInfoStorage
      .map(_.getOrElse(requestId, Vector.empty[Status]))
      .get
      .commit
      .map(statuses => PickupInfo(requestId, statuses))
  }

  private def updateStatus(requestId: UUID, status: Status): ZSTM[Any, Nothing, PickupInfo] = {
    pickupInfoStorage
      .updateAndGet { m =>
        val newValue = m.get(requestId).fold(Vector((status)))(v => status +: v)
        m + (requestId -> newValue)
      }
      .map(_.getOrElse(requestId, Vector.empty))
      .map(info => PickupInfo(requestId, info))
  }

  override def updatePickupStatus(requestId: UUID, status: Status): UIO[PickupInfo] = {
    updateStatus(requestId, status).commit
  }
}

object PickupServiceLive {
  val layer: URLayer[Has[TQueue[Pickup]] with Has[TRef[Map[UUID, Vector[Status]]]], Has[PickupService[UIO]]] =
    (PickupServiceLive(_, _)).toLayer
}
