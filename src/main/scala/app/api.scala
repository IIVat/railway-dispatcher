package app

import app.services.RailwayDispatcher
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import zio._
import zio.interop.catz._

import java.util.UUID

object api {
  trait PickupApi {
    def routes: HttpRoutes[Task]
  }

  object PickupApi {
    val pickupRoutes: URIO[Has[PickupApi], HttpRoutes[Task]] = ZIO.access(_.get.routes)
  }

  object PickupApiLive {
    val layer: ZLayer[Has[RailwayDispatcher[UIO]], Nothing, Has[PickupApi]] =
      ZLayer.fromService { dispatcher =>
        implicit def circeJsonDecoder[A](implicit decoder: Decoder[A]): EntityDecoder[Task, A] = jsonOf[Task, A]

        val dsl = Http4sDsl[Task]

        import dsl._

        new PickupApi {
          val routes = HttpRoutes.of[Task] {
            case req @ POST -> Root / "pickup" =>
              req.decode[PickupRequest] { req =>
                val reqId = UUID.randomUUID()
                (for {
                  pickup <- ZIO.fromEither(Pickup.validate(reqId, req.currentLocation, req.destination))
                  _      <- dispatcher.enqueuePickup(pickup)
                } yield reqId).foldM(msg => NotFound(msg), id => Ok(id.toString))
              }
            case GET -> Root / "status" / UUIDVar(reqId) =>
              dispatcher.getPickupStatus(reqId).flatMap(resp => Ok(resp.asJson))
          }
        }
      }
  }

  trait TrainsApi {
    def routes: HttpRoutes[Task]
  }

  object TrainsApi {
    val trainsRoutes: URIO[Has[TrainsApi], HttpRoutes[Task]] = ZIO.access(_.get.routes)
  }

  object TrainsApiLive {
    val layer: ZLayer[Has[RailwayDispatcher[UIO]], Nothing, Has[TrainsApi]] =
      ZLayer.fromService { dispatcher =>
        val dsl = Http4sDsl[Task]

        import dsl._

        new TrainsApi {
          val routes = HttpRoutes.of[Task] {
            case GET -> Root =>
              dispatcher.getTrains.flatMap(resp => Ok(resp.asJson))
          }
        }
      }
  }
}
