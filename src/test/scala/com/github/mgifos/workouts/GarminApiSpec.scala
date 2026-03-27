package com.github.mgifos.workouts

import java.time.LocalDate

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.http4s.*
import org.http4s.client.Client
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import com.github.mgifos.workouts.model.*

class GarminApiSpec extends AnyWordSpec with Matchers {

  private given GarminSession = GarminSession("test-token")

  /** Builds a mock [[Client]] from a partial function over (Method, path-string) pairs.
    * Unmatched requests return 500.
    */
  private def mockClient(
      pf: PartialFunction[(Method, String), IO[Response[IO]]]
  ): Client[IO] =
    Client.fromHttpApp(HttpApp[IO] { req =>
      pf.lift((req.method, req.uri.path.renderString))
        .getOrElse(IO.pure(Response[IO](Status.InternalServerError)))
    })

  private def jsonOk(body: String): IO[Response[IO]] =
    IO.pure(
      Response[IO](Status.Ok)
        .withEntity(body)
        .withHeaders(Headers(Header.Raw(org.typelevel.ci.CIString("Content-Type"), "application/json")))
    )

  private val sampleWorkout = WorkoutDef("running", "test-run", Seq(CooldownStep(LapButtonPressed)))

  "GarminApi.createWorkouts" should {

    "return a GarminWorkout with the server-assigned id on success" in {
      val client = mockClient { case (Method.POST, p) if p.endsWith("/workout") =>
        jsonOk("""{"workoutId": 42}""")
      }
      val result = GarminApi(client).createWorkouts(List(sampleWorkout)).unsafeRunSync()
      result shouldBe List(GarminWorkout("test-run", 42L))
    }

    "raise RuntimeException when the response status is not Ok" in {
      val client = mockClient { case (Method.POST, p) if p.endsWith("/workout") =>
        IO.pure(Response[IO](Status.Unauthorized))
      }
      an[RuntimeException] should be thrownBy
        GarminApi(client).createWorkouts(List(sampleWorkout)).unsafeRunSync()
    }

    "raise RuntimeException when the response body has no workoutId field" in {
      val client = mockClient { case (Method.POST, p) if p.endsWith("/workout") =>
        jsonOk("""{"unexpected": "payload"}""")
      }
      an[RuntimeException] should be thrownBy
        GarminApi(client).createWorkouts(List(sampleWorkout)).unsafeRunSync()
    }

    "return an empty list when given no workouts" in {
      val client = mockClient(PartialFunction.empty)
      val result = GarminApi(client).createWorkouts(List.empty).unsafeRunSync()
      result shouldBe empty
    }
  }

  "GarminApi.deleteWorkouts" should {

    "delete all matching workouts and return the group count" in {
      val listJson = """[{"workoutName":"test-run","workoutId":1},{"workoutName":"test-run","workoutId":2}]"""
      val client = mockClient {
        case (Method.GET, p) if p.contains("workouts") => jsonOk(listJson)
        case (Method.DELETE, _)                        => IO.pure(Response[IO](Status.NoContent))
      }
      GarminApi(client).deleteWorkouts(List("test-run")).unsafeRunSync() shouldBe 1
    }

    "skip workouts not found on Garmin and return 0" in {
      val client = mockClient { case (Method.GET, p) if p.contains("workouts") =>
        jsonOk("[]")
      }
      GarminApi(client).deleteWorkouts(List("ghost-workout")).unsafeRunSync() shouldBe 0
    }

    "raise RuntimeException when the workout list endpoint returns an error" in {
      val client = mockClient { case (Method.GET, p) if p.contains("workouts") =>
        IO.pure(Response[IO](Status.Unauthorized))
      }
      an[RuntimeException] should be thrownBy
        GarminApi(client).deleteWorkouts(List("test-run")).unsafeRunSync()
    }

    "handle malformed workout list JSON gracefully (treats as empty)" in {
      val client = mockClient { case (Method.GET, p) if p.contains("workouts") =>
        jsonOk("not-json")
      }
      // parseJson fails → getOrElse(Nil) → no workouts found → count 0
      GarminApi(client).deleteWorkouts(List("test-run")).unsafeRunSync() shouldBe 0
    }
  }

  "GarminApi.schedule" should {

    "schedule all items and return the count" in {
      val client = mockClient { case (Method.POST, p) if p.contains("schedule") =>
        IO.pure(Response[IO](Status.Ok))
      }
      val spec = List(LocalDate.of(2025, 1, 6) -> GarminWorkout("test-run", 99L))
      GarminApi(client).schedule(spec).unsafeRunSync() shouldBe 1
    }

    "still count items where scheduling returns a non-Ok status" in {
      val client = mockClient { case (Method.POST, p) if p.contains("schedule") =>
        IO.pure(Response[IO](Status.InternalServerError))
      }
      val spec = List(
        LocalDate.of(2025, 1, 6) -> GarminWorkout("run-a", 1L),
        LocalDate.of(2025, 1, 7) -> GarminWorkout("run-b", 2L)
      )
      GarminApi(client).schedule(spec).unsafeRunSync() shouldBe 2
    }

    "return 0 for an empty schedule" in {
      val client = mockClient(PartialFunction.empty)
      GarminApi(client).schedule(List.empty).unsafeRunSync() shouldBe 0
    }
  }
}
