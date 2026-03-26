package com.github.mgifos.workouts

import java.time.LocalDate

import cats.effect.{IO, Ref}
import cats.effect.std.Mutex
import cats.syntax.traverse._
import com.github.mgifos.workouts.model.WorkoutDef
import com.typesafe.scalalogging.Logger
import fs2.Stream
import io.circe.parser.{parse => parseJson}
import org.http4s._
import org.http4s.client.Client
import org.http4s.headers._
import org.typelevel.ci.CIString

import scala.concurrent.duration._

case class GarminWorkout(name: String, id: Long)

case class GarminSession(cookies: List[RequestCookie])

class GarminConnect(
    email: String,
    password: String,
    client: Client[IO],
    sessionRef: Ref[IO, Option[GarminSession]],
    loginMutex: Mutex[IO]
) {

  private val log = Logger(getClass)

  def createWorkouts(workouts: List[WorkoutDef])(implicit session: GarminSession): IO[List[GarminWorkout]] = {
    log.info("\nCreating workouts:")
    Stream
      .emits(workouts)
      .metered[IO](1.second)
      .evalMap { workout =>
        val req = Request[IO](
          method = Method.POST,
          uri = Uri.unsafeFromString("https://connect.garmin.com/modern/proxy/workout-service/workout")
        ).withEntity(workout.json().noSpaces)
          .withHeaders(
            sessionHeaders(session)
              ++ List(
                Header.Raw(CIString("Content-Type"), "application/json"),
                Header.Raw(CIString("Referer"), "https://connect.garmin.com/modern/workout/create/running"),
                Header.Raw(CIString("NK"), "NT")
              )
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

  def deleteWorkouts(workouts: List[String])(implicit session: GarminSession): IO[Int] =
    for {
      wsMap <- getWorkoutsMap()
      _     <- IO(log.info("\nDeleting workouts:"))
      reqs: List[(String, List[Request[IO]])] = for {
               workout <- workouts
               ids = wsMap.getOrElse(workout, List.empty[Long])
               if ids.nonEmpty
             } yield {
               val label = s"$workout -> ${ids.mkString("[", ", ", "]")}"
               label -> ids.map { id =>
                 Request[IO](
                   method = Method.POST,
                   uri = Uri.unsafeFromString(s"https://connect.garmin.com/modern/proxy/workout-service/workout/$id")
                 ).withHeaders(
                   sessionHeaders(session)
                     ++ List(
                       Header.Raw(CIString("Referer"), "https://connect.garmin.com/modern/workouts"),
                       Header.Raw(CIString("NK"), "NT"),
                       Header.Raw(CIString("X-HTTP-Method-Override"), "DELETE")
                     )
                 )
               }
             }
      count <- Stream
                 .emits(reqs)
                 .metered[IO](1.second)
                 .evalMap { case (label, perIdReqs) =>
                   perIdReqs
                     .traverse(req => client.run(req).use(resp => IO.pure(resp.status)))
                     .map { statuses =>
                       if (statuses.forall(_ == Status.NoContent)) log.info(s"  $label")
                       else log.error(s"  Cannot delete workout: $label")
                     }
                 }
                 .compile
                 .toList
                 .map(_.length)
    } yield count

  def schedule(spec: List[(LocalDate, GarminWorkout)])(implicit session: GarminSession): IO[Int] = {
    log.debug(s"  Scheduling spec: ${spec.mkString("\n")}")
    log.info("\nScheduling:")
    Stream
      .emits(spec)
      .metered[IO](1.second)
      .evalMap { case (date, gw) =>
        val label = s"$date -> ${gw.name}"
        val body  = io.circe.Json.obj("date" -> io.circe.Json.fromString(date.toString))
        val req = Request[IO](
          method = Method.POST,
          uri = Uri.unsafeFromString(s"https://connect.garmin.com/modern/proxy/workout-service/schedule/${gw.id}")
        ).withEntity(body.noSpaces)
          .withHeaders(
            sessionHeaders(session)
              ++ List(
                Header.Raw(CIString("Content-Type"), "application/json"),
                Header.Raw(CIString("Referer"), "https://connect.garmin.com/modern/calendar"),
                Header.Raw(CIString("NK"), "NT")
              )
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

  private def getWorkoutsMap()(implicit session: GarminSession): IO[Map[String, List[Long]]] = {
    val req = Request[IO](
      method = Method.GET,
      uri = Uri.unsafeFromString(
        "https://connect.garmin.com/modern/proxy/workout-service/workouts?start=1&limit=9999&myWorkoutsOnly=true&sharedWorkoutsOnly=false"
      )
    ).withHeaders(
      sessionHeaders(session)
        ++ List(
          Header.Raw(CIString("Referer"), "https://connect.garmin.com/modern/workouts"),
          Header.Raw(CIString("NK"), "NT")
        )
    )
    client.run(req).use { resp =>
      if (resp.status == Status.Ok)
        resp.as[String].map { json =>
          parseJson(json)
            .flatMap(_.as[List[io.circe.Json]])
            .getOrElse(Nil)
            .flatMap { obj =>
              for {
                name <- obj.hcursor.get[String]("workoutName").toOption
                id   <- obj.hcursor.get[Long]("workoutId").toOption
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

  def login(forceNewSession: Boolean = false): IO[Either[String, GarminSession]] =
    loginMutex.lock.surround {
      sessionRef.get.flatMap {
        case Some(s) if !forceNewSession => IO.pure(Right(s))
        case _ =>
          performLogin
            .flatMap { session =>
              val valid = session.cookies.exists(_.content.matches("""SESSIONID=[a-z\d-]{5,}"""))
              if (valid)
                sessionRef.set(Some(session)) *>
                  IO(log.info("Successfully logged in to Garmin Connect!")) *>
                  IO.pure(Right(session): Either[String, GarminSession])
              else
                IO.pure(Left("Login was not successful, check your username and password and try again."))
            }
            .handleError { _ =>
              Left("Attempt to log in to Garmin Connect was not successful (this could be a server error).")
            }
      }
    }

  private def performLogin: IO[GarminSession] = {

    def extractCookies(resp: Response[IO]): List[RequestCookie] =
      resp.cookies.map(rc => RequestCookie(rc.name, rc.content))

    def cookieHeader(cookies: List[RequestCookie]): List[Header.Raw] =
      if (cookies.isEmpty) Nil
      else List(Header.Raw(CIString("Cookie"), cookies.map(c => s"${c.name}=${c.content}").mkString("; ")))

    def redirectionLoop(count: Int, url: Uri, acc: List[RequestCookie]): IO[List[RequestCookie]] =
      client
        .run(
          Request[IO](Method.GET, url)
            .withHeaders(cookieHeader(acc))
        )
        .use { resp =>
          val newCookies = extractCookies(resp)
          resp.headers.get[Location] match {
            case Some(Location(loc)) if count < 7 =>
              val nextUri = if (loc.path.renderString.startsWith("/"))
                Uri(scheme = loc.scheme, authority = loc.authority, path = loc.path, query = loc.query)
              else loc
              redirectionLoop(count + 1, nextUri, acc ++ newCookies)
            case _ => IO.pure(acc ++ newCookies)
          }
        }

    val params = Map(
      "clientId"             -> "GarminConnect",
      "consumeServiceTicket" -> "false",
      "gauthHost"            -> "https://sso.garmin.com/sso",
      "service"              -> "https://connect.garmin.com/modern"
    )

    val loginUri  = Uri.unsafeFromString("https://sso.garmin.com/sso/login").withQueryParams(params)
    val signinUri = Uri.unsafeFromString("https://sso.garmin.com/sso/signin").withQueryParams(params)

    for {
      cookies1 <- client
                    .run(Request[IO](Method.GET, loginUri))
                    .use(resp => IO.pure(extractCookies(resp)))
      cookies2 <- client
                    .run(
                      Request[IO](Method.POST, signinUri)
                        .withEntity(
                          UrlForm(
                            "username" -> email,
                            "password" -> password,
                            "embed"    -> "false"
                          )
                        )
                        .withHeaders(
                          cookieHeader(cookies1)
                            :+ Header.Raw(CIString("Origin"), "https://sso.garmin.com")
                        )
                    )
                    .use(resp => IO.pure(extractCookies(resp)))
      sessionCookies <- redirectionLoop(0, Uri.unsafeFromString("https://connect.garmin.com/modern"), cookies2)
    } yield GarminSession(sessionCookies)
  }

  private def sessionHeaders(session: GarminSession): List[Header.Raw] =
    if (session.cookies.isEmpty) Nil
    else List(Header.Raw(CIString("Cookie"), session.cookies.map(c => s"${c.name}=${c.content}").mkString("; ")))
}

object GarminConnect {
  def make(email: String, password: String, client: Client[IO]): IO[GarminConnect] =
    for {
      ref   <- Ref.of[IO, Option[GarminSession]](None)
      mutex <- Mutex[IO]
    } yield new GarminConnect(email, password, client, ref, mutex)
}
