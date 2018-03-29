package com.github.mgifos.workouts

import java.time.LocalDate

import akka.actor.{ Actor, ActorRef, ActorSystem, Props }
import akka.http.scaladsl.Http
import akka.http.scaladsl.client.RequestBuilding._
import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.pattern.ask
import akka.stream.scaladsl.{ Flow, Sink, Source }
import akka.stream.{ Materializer, ThrottleMode }
import akka.util.Timeout
import com.github.mgifos.workouts.GarminConnect._
import com.github.mgifos.workouts.model.WorkoutDef
import com.typesafe.scalalogging.Logger
import play.api.libs.json.{ JsObject, Json }

import scala.collection.immutable.{ Map, Seq }
import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }
import scala.language.implicitConversions
import scala.util.Failure

case class GarminWorkout(name: String, id: Long)

class GarminConnect(email: String, password: String)(implicit system: ActorSystem, executionContext: ExecutionContext, mat: Materializer) {

  case class Session(username: String, headers: Seq[HttpHeader])

  case class Login(forceNewSession: Boolean)

  private val log = Logger(getClass)

  /**
   * Creates workout definitions
   *
   * @param workouts
   * @return
   */
  def createWorkouts(workouts: Seq[WorkoutDef]): Future[Seq[GarminWorkout]] = {

    val source = Source.fromFuture(login()).flatMapConcat { session =>
      log.info("\nCreating workouts:")
      Source(workouts.map { workout =>
        val req = Post("https://connect.garmin.com/modern/proxy/workout-service/workout")
          .withEntity(HttpEntity(`application/json`, workout.json.toString()))
          .withHeaders(session.headers
            :+ Referer("https://connect.garmin.com/modern/workout/create/running")
            :+ RawHeader("NK", "NT"))
        workout.name -> req
      })
    }

    val flow = Flow[(String, HttpRequest)]
      .throttle(1, 1.second, 1, ThrottleMode.shaping)
      .mapAsync(1) {
        case (workout, req) =>
          Http().singleRequest(req).flatMap { res =>
            if (res.status == OK) {
              res.body.map { json =>
                log.info(s"  $workout")
                GarminWorkout(workout, Json.parse(json).\("workoutId").as[Long])
              }
            } else {
              log.debug(s"Creation wo response: $res")
              Future.failed(new Error("Cannot create workout"))
            }
          }
      }

    source.via(flow).runWith(Sink.seq)
  }

  /**
   * Deletes workouts with provided names
   *
   * @param workouts Workout names
   * @return Count of deleted items
   */
  def deleteWorkouts(workouts: Seq[String]): Future[Int] = {

    val futureRequests = for {
      session <- login()
      map <- getWorkoutsMap()
    } yield {
      log.info("\nDeleting workouts:")
      for {
        workout <- workouts
        ids = map.getOrElse(workout, Seq.empty)
        if ids.nonEmpty
      } yield {
        val label = s"$workout -> ${ids.mkString("[", ", ", "]")}"
        label -> ids.map(id => Post(s"https://connect.garmin.com/modern/proxy/workout-service/workout/$id")
          .withHeaders(session.headers
            :+ Referer("https://connect.garmin.com/modern/workouts")
            :+ RawHeader("NK", "NT")
            :+ RawHeader("X-HTTP-Method-Override", "DELETE")))
      }
    }
    val source = Source.fromFuture(futureRequests).flatMapConcat(Source(_))
      .throttle(1, 1.second, 1, ThrottleMode.shaping)
      .mapAsync(1) {
        case (label, reqs) =>
          val statusesFut = Future.sequence(reqs.map(req => Http().singleRequest(req).withoutBody))
          statusesFut.map { statuses =>
            if (statuses.forall(_.status == NoContent)) log.info(s"  $label")
            else log.error(s"  Cannot delete workout: $label")
          }
      }
    source.runWith(Sink.seq).map(_.length)
  }

  def schedule(spec: Seq[(LocalDate, GarminWorkout)]): Future[Int] = {
    log.debug(s"  Scheduling spec: ${spec.mkString("\n")}")
    Source.fromFuture(login(forceNewSession = true)).flatMapConcat { session =>
      log.info("\nScheduling:")
      Source(spec).map {
        case (date, gw) =>
          s"$date -> ${gw.name}" -> Post(s"https://connect.garmin.com/modern/proxy/workout-service/schedule/${gw.id}")
            .withHeaders(session.headers
              :+ Referer("https://connect.garmin.com/modern/calendar")
              :+ RawHeader("NK", "NT"))
            .withEntity(HttpEntity(`application/json`, Json.obj("date" -> date.toString).toString))
      }.throttle(1, 1.second, 1, ThrottleMode.shaping)
        .mapAsync(1) {
          case (label, req) =>
            Http().singleRequest(req).withoutBody.map { res =>
              log.debug(s"  Received $res")
              if (res.status == OK) log.info(s"  $label")
              else log.error(s"  Cannot schedule: $label")
            }
        }
    }.runWith(Sink.seq).map(_.length)
  }

  /**
   * Retrieves workout mapping: name -> Seq[id] @ GarminConnect
   * @return
   */
  private def getWorkoutsMap(): Future[Map[String, Seq[Long]]] = {
    val source = Source.fromFuture(login()).flatMapConcat { session =>
      val req = Get("https://connect.garmin.com/modern/proxy/workout-service/workouts?start=1&limit=9999&myWorkoutsOnly=true&sharedWorkoutsOnly=false")
        .withHeaders(session.headers
          :+ Referer("https://connect.garmin.com/modern/workouts")
          :+ RawHeader("NK", "NT"))
      Source.fromFuture(
        Http().singleRequest(req).flatMap { res =>
          if (res.status == OK)
            res.body.map { json =>
              val x = Json.parse(json).asOpt[Seq[JsObject]].map { arr =>
                arr.map(x => (x \ "workoutName").as[String] -> (x \ "workoutId").as[Long])
              }.getOrElse(Seq.empty)
              x.groupBy {
                case (name, _) => name
              }.map {
                case (a, b) => a -> b.map(_._2)
              }
            }
          else {
            log.debug(s"Cannot retrieve workout list, response: $res")
            Future.failed(new Error("Cannot retrieve workout list from Garmin Connect"))
          }
        })
    }
    source.runWith(Sink.head)
  }

  private lazy val loginActor: ActorRef = system.actorOf(Props(new LoginActor()))

  private def login(forceNewSession: Boolean = false): Future[Session] = ask(loginActor, Login(forceNewSession))(Timeout(2.minutes)).mapTo[Session]

  /**
   * Holds and reloads session if neccessary
   */
  class LoginActor extends Actor {

    private case class NewSession(session: Session)

    var maybeSession: Option[Session] = None

    override def receive = {

      case Login(force) =>
        val origin = sender()
        maybeSession match {
          case Some(s) if !force => origin ! s
          case _ =>
            login.andThen {
              case util.Success(x) =>
                origin ! x
                self ! NewSession(x)
              case Failure(_) =>
                log.error("Failed to log in to Garmin Connect")
            }
        }

      case NewSession(session) =>
        if (maybeSession.isEmpty) log.info("Successfully logged in to Garmin Connect")
        maybeSession = Option(session)
    }

    private def login: Future[Session] = {

      def extractCookies(res: HttpResponse) = res.headers.collect { case x: `Set-Cookie` => x.cookie }.map(c => Cookie(c.name, c.value))

      def redirectionLoop(count: Int, url: String, acc: Seq[Cookie]): Future[Seq[Cookie]] = {
        Http().singleRequest {
          HttpRequest(uri = Uri(url)).withHeaders(acc)
        }.withoutBody.flatMap { res =>
          val cookies = extractCookies(res)
          res.headers.find(_.name() == "Location") match {
            case Some(header) =>
              if (count < 7) {
                val path = header.value()
                val base = path.split("/").take(3).mkString("/")
                val nextUrl = if (path.startsWith("/")) base + path else path
                redirectionLoop(count + 1, nextUrl, acc ++ cookies)
              } else {
                Future.successful(acc ++ cookies)
              }
            case None => Future.successful(acc ++ cookies)
          }
        }
      }

      val params = Map(
        "service" -> "https://connect.garmin.com/post-auth/login",
        "clientId" -> "GarminConnect",
        "gauthHost" -> "https://sso.garmin.com/sso",
        "consumeServiceTicket" -> "false")
      for {
        res1 <- Http().singleRequest(HttpRequest(uri = Uri("https://sso.garmin.com/sso/login").withQuery(Query(params)))).withoutBody
        res2 <- Http().singleRequest(
          HttpRequest(
            POST,
            Uri("https://sso.garmin.com/sso/login").withQuery(Query(params)),
            entity = FormData(Map(
              "username" -> email,
              "password" -> password,
              "_eventId" -> "submit",
              "embed" -> "true")).toEntity).withHeaders(extractCookies(res1))).withoutBody
        sessionCookies <- redirectionLoop(0, "https://connect.garmin.com/post-auth/login", extractCookies(res2))
        username <- getUsername(sessionCookies)
      } yield Session(username, sessionCookies)
    }

    private def getUsername(sessionCookies: Seq[HttpHeader]): Future[String] = {
      val req = HttpRequest(GET, Uri("https://connect.garmin.com/user/username")).withHeaders(sessionCookies)
      Http().singleRequest(req).flatMap { res =>
        if (res.status != StatusCodes.OK) throw new Error("Login failed!")
        res.body.map { json =>
          (Json.parse(json) \ "username").as[String]
        }
      }
    }
  }
}

object GarminConnect {

  class HttpResponseWithBody(original: HttpResponse) {
    def body(implicit ec: ExecutionContext, mat: Materializer): Future[String] = original.entity.toStrict(10.seconds).map(_.data.utf8String)
  }

  class LiteHttpFuture(original: Future[HttpResponse]) {
    def withoutBody(implicit ec: ExecutionContext, mat: Materializer) = original.andThen {
      case util.Success(res) => res.discardEntityBytes()
    }
  }

  implicit def responseWithBody(original: HttpResponse): HttpResponseWithBody = new HttpResponseWithBody(original)
  implicit def liteHttpFuture(original: Future[HttpResponse]): LiteHttpFuture = new LiteHttpFuture(original)
}
