package services

import app.Pickup
import app.Status
import app.services.PickupService
import app.services.PickupServiceLive
import zio.stm.TQueue
import zio.stm.TRef
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.DefaultRunnableSpec

import java.util.UUID

object PickupServiceSpec extends DefaultRunnableSpec {
  private val layer: ULayer[Has[PickupService[UIO]]] =
    (
      TQueue.bounded[Pickup](10).commit.toLayer ++
        TRef.makeCommit(Map.empty[UUID, Vector[Status]]).toLayer
    ) >>> PickupServiceLive.layer

  override def spec = {
    suite("LivePickupServiceSpec")(testM("Successfully enqueues requests, updates statuses and then dequeue requests") {
      val uuid1 = UUID.randomUUID()
      val uuid2 = UUID.randomUUID()
      for {
        _        <- PickupService.enqueue(Pickup.validate(uuid1, 2, 3).toOption.get)
        _        <- PickupService.enqueue(Pickup.validate(uuid2, 1, 5).toOption.get)
        queued1  <- PickupService.getPickupStatus(uuid1)
        queued2  <- PickupService.getPickupStatus(uuid2)
        _        <- PickupService.updatePickupStatus(uuid1, Status.Waiting)
        _        <- PickupService.updatePickupStatus(uuid2, Status.PickedUp(3, 1))
        waiting  <- PickupService.getPickupStatus(uuid1)
        pickedup <- PickupService.getPickupStatus(uuid2)
        res      <- PickupService.dequeueUpTo(3)
      } yield {
        assert(queued1.statuses.head)(equalTo(Status.Queued)) &&
        assert(queued2.statuses.head)(equalTo(Status.Queued)) &&
        assert(waiting.statuses.head)(equalTo(Status.Waiting)) &&
        assert(pickedup.statuses.head)(equalTo(Status.PickedUp(3, 1))) &&
        assert(res.size)(equalTo(2))
      }
    }.provideLayer(layer))
  }
}
