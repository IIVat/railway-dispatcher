package app

import app.api.PickupApiLive
import app.api.TrainsApiLive
import app.services.*
import org.http4s.implicits.*
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import zio.*
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.*
import zio.duration.durationInt
import zio.interop.catz.*
import zio.interop.catz.implicits.*
import zio.magic.*
import zio.random.Random
import zio.stm.TQueue
import zio.stm.TRef

import java.util.UUID

object RailwayApp extends zio.App {

  //trains == lines
  val trainsQuantity              = 32
  val numberOfDispatchingRequests = 32
  val queueCapacity               = 1024

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {

    val requestsQueue = TQueue.bounded[Pickup](queueCapacity).commit.toLayer
    val pickupInfoRef = TRef.makeCommit[Map[UUID, Vector[Status]]](Map.empty).toLayer
    val trainsRef     = TRef.makeCommit[Vector[Train]](Vector.empty).toLayer

    val program = for {
      _                <- putStrLn("[railway-info] Dispatcher is registering trains ...")
      registeredTrains <- RailwayDispatcher.registerTrains(trainsQuantity)
      _                <- putStrLn(s"[railway-info] $registeredTrains trains were registered")
      _                <- putStrLn("[railway-info] Dispatcher is starting ...")
      _                <- RailwayDispatcher.dispatch(numberOfDispatchingRequests, 1.second, 100.millis).forkDaemon
      _                <- putStrLn("[railway-info] Server is starting...")
      pickupRoutes     <- api.PickupApi.pickupRoutes
      trainsRoutes     <- api.TrainsApi.trainsRoutes

      _ <- ZIO
        .runtime[ZEnv]
        .flatMap { implicit runtime =>
          BlazeServerBuilder[Task](runtime.platform.executor.asEC)
            .bindHttp(9000, "localhost")
            .withHttpApp(Router("pickups" -> pickupRoutes, "trains" -> trainsRoutes).orNotFound)
            .serve
            .compile
            .drain
        }
      _ <- putStrLn("[railway-info] Dispatcher can receive pickup requests on localhost:9000/pickup")
    } yield ()

    program
      .inject(
        RailwayDispatcherLive.layer,
        PickupApiLive.layer,
        TrainsApiLive.layer,
        PickupServiceLive.layer,
        TrainsServiceLive.layer,
        requestsQueue,
        pickupInfoRef,
        trainsRef,
        Clock.live,
        Console.live,
        Random.live,
        Blocking.live,
        zio.system.System.live
      )
      .exitCode
  }
}
