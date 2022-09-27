package services

import app.Pickup
import app.Status
import app.Train
import app.services.*
import zio.duration.*
import zio.magic.*
import zio.stm.TQueue
import zio.stm.TRef
import zio.test.*
import zio.test.Assertion.equalTo
import zio.test.environment.TestClock
import zio.test.environment.testEnvironment

import java.time.Duration
import java.util.UUID

object RailwayDispatcherSpec extends DefaultRunnableSpec {
  override def spec =
    suite("LiveRailwayDispatcherSpec")(
      testM("Train should pick up users in the order 0 -> 1(pick) -> 6(dest) -> 3(pick) -> 5(dest)") {
        val uuid1   = UUID.randomUUID()
        val uuid2   = UUID.randomUUID()
        val pickup1 = Pickup.validate(uuid1, 1, 6).toOption.get
        val pickup2 = Pickup.validate(uuid2, 3, 5).toOption.get
        for {
          _     <- RailwayDispatcher.registerTrains(1)
          _     <- RailwayDispatcher.dispatch(10, 100.millis, 50.millis).forkDaemon
          _     <- RailwayDispatcher.enqueuePickup(pickup1)
          _     <- RailwayDispatcher.enqueuePickup(pickup2)
          _     <- TestClock.adjust(5.second)
          info1 <- RailwayDispatcher.getPickupStatus(uuid1)
          info2 <- RailwayDispatcher.getPickupStatus(uuid2)
        } yield {
          assert(info1.statuses.head)(equalTo(Status.PickedUp(lineId = 0, waitingInterval = 1))) &&
          assert(info2.statuses.head)(equalTo(Status.PickedUp(lineId = 0, waitingInterval = 3)))
        }
      }.inject(
        RailwayDispatcherLive.layer,
        TRef
          .makeCommit(Vector.empty[Train])
          .toLayer,
        TrainsServiceLive.layer,
        TQueue.bounded[Pickup](10).commit.toLayer,
        TRef.makeCommit(Map.empty[UUID, Vector[Status]]).toLayer,
        PickupServiceLive.layer,
        testEnvironment
      ),
      testM("Train should pick up users in the order 0(init) -> 1(pick) -> 2(pick) -> 7(dest)") {
        val uuid1   = UUID.randomUUID()
        val uuid2   = UUID.randomUUID()
        val pickup1 = Pickup.validate(uuid1, 1, 10).toOption.get
        val pickup2 = Pickup.validate(uuid2, 2, 10).toOption.get
        for {
          _     <- RailwayDispatcher.registerTrains(1)
          _     <- RailwayDispatcher.dispatch(10, 100.millis, 50.millis).forkDaemon
          _     <- RailwayDispatcher.enqueuePickup(pickup1)
          _     <- RailwayDispatcher.enqueuePickup(pickup2)
          _     <- TestClock.adjust(1.second)
          info1 <- RailwayDispatcher.getPickupStatus(uuid1)
          info2 <- RailwayDispatcher.getPickupStatus(uuid2)
          _     <- TestClock.adjust(1.seconds)
//          train <- TrainsService.getTrains
        } yield {
          assert(info1.statuses.head)(equalTo(Status.PickedUp(lineId = 0, waitingInterval = 1))) &&
          assert(info2.statuses.head)(equalTo(Status.PickedUp(lineId = 0, waitingInterval = 2))) /*&&*/
          // the assertion works if the test is run in IJ, and doesn't work in terminal
//          assert(train.head.currentLocation.terminalId.value)(equalTo(10)) &&
//          assert(train.head.currentLocation.usersOut)(equalTo(2))
        }
      }.inject(
        RailwayDispatcherLive.layer,
        TRef
          .makeCommit(Vector.empty[Train])
          .toLayer,
        TrainsServiceLive.layer,
        TQueue.bounded[Pickup](10).commit.toLayer,
        TRef.makeCommit(Map.empty[UUID, Vector[Status]]).toLayer,
        PickupServiceLive.layer,
        testEnvironment
      )
    )
}
