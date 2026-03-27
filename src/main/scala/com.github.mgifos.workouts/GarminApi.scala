package com.github.mgifos.workouts

import java.time.LocalDate

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.traverse.*
import com.typesafe.scalalogging.Logger
import fs2.Stream
import io.circe.parser.parse as parseJson
import org.http4s.*
import org.http4s.client.Client
import org.typelevel.ci.CIString

import com.github.mgifos.workouts.model.WorkoutDef

/**
 * HTTP client for the Garmin Connect workout API. Provides operations to create and delete workout
 * definitions and to schedule them on the calendar. All methods require an implicit
 * [[GarminSession]] obtained from [[GarminAuth]].
 */
class GarminApi(client: Client[IO]) {

  private val log = Logger(getClass)
  private val workoutApiBase = "https://connectapi.garmin.com/workout-service"

  def createWorkouts(
      workouts: List[WorkoutDef]
  )(using session: GarminSession): IO[List[GarminWorkout]] = {
    log.info("\nCreating workouts:")
    Stream
      .emits(workouts)
      .metered[IO](1.second)
      .evalMap { workout =>
        val req = Request[IO](
          method = Method.POST,
          uri = Uri.unsafeFromString(s"$workoutApiBase/workout")
        ).withEntity(workout.json.noSpaces)
          .withHeaders(
            sessionHeaders(session)
              :+ Header.Raw(CIString("Content-Type"), "application/json")
          )
        client.run(req).use { resp =>
          if (resp.status == Status.Ok)
            resp.as[String].flatMap { json =>
              parseJson(json).flatMap(_.hcursor.get[Long]("workoutId")) match {
                case Right(id) =>
                  IO(log.info(s"  ${workout.name}")).as(GarminWorkout(workout.name, id))
                case Left(e) =>
                  IO.raiseError(new RuntimeException(s"Cannot parse workoutId: $e"))
              }
            }
          else {
            log.debug(s"Creation wo response: ${resp.status}")
            IO.raiseError(new RuntimeException("Cannot create workout"))
          }
        }
      }
      .compile
      .toList
  }

  def deleteWorkouts(workouts: List[String])(using session: GarminSession): IO[Int] =
    for {
      wsMap <- getWorkoutsMap()
      _ <- IO(log.info("\nDeleting workouts:"))
      reqs: List[(String, List[Request[IO]])] = for {
        workout <- workouts
        ids = wsMap.getOrElse(workout, List.empty[Long])
        if ids.nonEmpty
      } yield {
        val label = s"$workout -> ${ids.mkString("[", ", ", "]")}"
        label -> ids.map { id =>
          Request[IO](
            method = Method.DELETE,
            uri = Uri.unsafeFromString(s"$workoutApiBase/workout/$id")
          ).withHeaders(sessionHeaders(session))
        }
      }
      count <- Stream
        .emits(reqs)
        .metered[IO](1.second)
        .evalMap { case (label, perIdReqs) =>
          perIdReqs.traverse(req => client.run(req).use(resp => IO.pure(resp.status))).map {
            statuses =>
              if (statuses.forall(_ == Status.NoContent)) log.info(s"  $label")
              else log.error(s"  Cannot delete workout: $label")
          }
        }
        .compile
        .toList
        .map(_.length)
    } yield count

  def schedule(spec: List[(LocalDate, GarminWorkout)])(using session: GarminSession): IO[Int] = {
    log.debug(s"  Scheduling spec: ${spec.mkString("\n")}")
    log.info("\nScheduling:")
    Stream
      .emits(spec)
      .metered[IO](1.second)
      .evalMap { case (date, gw) =>
        val label = s"$date -> ${gw.name}"
        val body = io.circe.Json.obj("date" -> io.circe.Json.fromString(date.toString))
        val req = Request[IO](
          method = Method.POST,
          uri = Uri.unsafeFromString(s"$workoutApiBase/schedule/${gw.id}")
        ).withEntity(body.noSpaces)
          .withHeaders(
            sessionHeaders(session)
              :+ Header.Raw(CIString("Content-Type"), "application/json")
          )
        client.run(req).use { resp =>
          IO {
            log.debug(s"  Received ${resp.status}")
            if (resp.status == Status.Ok) log.info(s"  $label")
            else log.error(s"  Cannot schedule: $label")
          }
        }
      }
      .compile
      .toList
      .map(_.length)
  }

  private def getWorkoutsMap()(using session: GarminSession): IO[Map[String, List[Long]]] = {
    val req = Request[IO](
      method = Method.GET,
      uri = Uri.unsafeFromString(s"$workoutApiBase/workouts?start=1&limit=9999")
    ).withHeaders(sessionHeaders(session))
    client.run(req).use { resp =>
      if (resp.status == Status.Ok)
        resp.as[String].map { json =>
          parseJson(json)
            .flatMap(_.as[List[io.circe.Json]])
            .getOrElse(Nil)
            .flatMap { obj =>
              for {
                name <- obj.hcursor.get[String]("workoutName").toOption
                id <- obj.hcursor.get[Long]("workoutId").toOption
              } yield name -> id
            }
            .groupBy(_._1)
            .map { case (name, pairs) => name -> pairs.map(_._2) }
        }
      else {
        log.debug(s"Cannot retrieve workout list, response: ${resp.status}")
        IO.raiseError(new RuntimeException("Cannot retrieve workout list from Garmin Connect"))
      }
    }
  }

  private def sessionHeaders(session: GarminSession): List[Header.Raw] =
    List(
      Header.Raw(CIString("Authorization"), s"Bearer ${session.accessToken}"),
      Header.Raw(CIString("User-Agent"), "GCM-iOS-5.22.1.4")
    )
}
