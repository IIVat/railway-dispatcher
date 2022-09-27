package services

import app.Pickup
import app.Terminal
import app.TerminalId
import app.Train
import app.services.TrainsService
import app.services.TrainsServiceLive
import zio.*
import zio.stm.TRef
import zio.test.*
import zio.test.Assertion.*

import java.util.UUID

object TrainsServiceSpec extends DefaultRunnableSpec {
  private val layer: ULayer[Has[TrainsService[UIO]]] = TRef
    .makeCommit(Vector.empty[Train])
    .toLayer >>> TrainsServiceLive.layer

  override def spec = {
    suite("LiveTrainsServiceSpec")(
      testM("Successfully registers 5 trains") {
        TrainsService.addTrains(5).map(t => assert(t)(equalTo(5)))
      }.provideLayer(layer),
      testM("Successfully registers 32 trains") {
        TrainsService.addTrains(32).map(t => assert(t)(equalTo(32)))
      }.provideLayer(layer),
      testM("Should register 32 trains even if the given quantity is more than 32") {
        TrainsService.addTrains(50).map(t => assert(t)(equalTo(32)))
      }.provideLayer(layer),
      testM("Should get the same number of trains from storage after registration") {
        for {
          _      <- TrainsService.addTrains(5)
          trains <- TrainsService.getTrains
        } yield assert(trains.size)(equalTo(5))
      }.provideLayer(layer),
      testM("Should update the train state") {
        for {
          _            <- TrainsService.addTrains(1)
          initTrain    <- TrainsService.getTrains.map(_.head)
          _            <- TrainsService.updateState(Pickup.validate(UUID.randomUUID(), 2, 3).toOption.get)
          updatedTrain <- TrainsService.getTrains.map(_.head)
        } yield {
          val expected = Vector(Terminal(TerminalId(2), usersIn = 1, usersOut = 0),
                                Terminal(TerminalId(3), usersIn = 0, usersOut = 1))
          assert(initTrain.currentLocation.terminalId.value)(equalTo(0)) &&
          assert(initTrain.stops)(isEmpty) &&
          assert(updatedTrain.stops)(equalTo(expected))
        }
      }.provideLayer(layer),
      testM("Should move the train from T0 to T3") {
        for {
          _          <- TrainsService.addTrains(1)
          _          <- TrainsService.updateState(Pickup.validate(UUID.randomUUID(), 2, 3).toOption.get)
          initTrain  <- TrainsService.getTrains.map(_.head)
          _          <- TrainsService.moveTrains
          _          <- TrainsService.moveTrains
          _          <- TrainsService.moveTrains
          movedTrain <- TrainsService.getTrains.map(_.head)
        } yield {
          assert(initTrain.currentLocation.terminalId.value)(equalTo(0)) &&
          assert(initTrain.stops)(isNonEmpty) &&
          assert(movedTrain.stops)(isEmpty) &&
          assert(movedTrain.currentLocation.terminalId.value)(equalTo(3))
        }
      }.provideLayer(layer),
      testM("Should move the train from T0 to T3 and then to T1") {
        for {
          _          <- TrainsService.addTrains(1)
          _          <- TrainsService.updateState(Pickup.validate(UUID.randomUUID(), 3, 1).toOption.get)
          initTrain  <- TrainsService.getTrains.map(_.head)
          _          <- TrainsService.moveTrains
          _          <- TrainsService.moveTrains
          _          <- TrainsService.moveTrains
          _          <- TrainsService.moveTrains
          _          <- TrainsService.moveTrains
          movedTrain <- TrainsService.getTrains.map(_.head)
        } yield {
          assert(initTrain.currentLocation.terminalId.value)(equalTo(0)) &&
          assert(initTrain.stops)(isNonEmpty) &&
          assert(movedTrain.stops)(isEmpty) &&
          assert(movedTrain.currentLocation.terminalId.value)(equalTo(1))
        }
      }.provideLayer(layer)
    )
  }
}
