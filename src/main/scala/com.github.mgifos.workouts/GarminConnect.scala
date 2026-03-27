package com.github.mgifos.workouts

import java.net.URLDecoder
import java.net.URLEncoder
import java.time.LocalDate
import java.util.Base64
import java.util.UUID
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.Ref
import cats.effect.std.Mutex
import cats.syntax.traverse.*
import com.typesafe.scalalogging.Logger
import fs2.Stream
import io.circe.parser.parse as parseJson
import org.http4s.*
import org.http4s.client.Client
import org.typelevel.ci.CIString

import com.github.mgifos.workouts.model.WorkoutDef

case class GarminWorkout(name: String, id: Long)

case class GarminSession(accessToken: String)

class GarminConnect(
  email: String,
  password: String,
  client: Client[IO],
  sessionRef: Ref[IO, Option[GarminSession]],
  loginMutex: Mutex[IO]
) {

  private val log = Logger(getClass)

  private val workoutApiBase = "https://connectapi.garmin.com/workout-service"
  private val oauthApiBase = "https://connectapi.garmin.com/oauth-service/oauth"

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

  def login(forceNewSession: Boolean = false): IO[Either[String, GarminSession]] =
    loginMutex.lock.surround {
      sessionRef.get.flatMap {
        case Some(s) if !forceNewSession => IO.pure(Right(s))
        case _ =>
          performLogin.flatMap { session =>
            val valid = session.accessToken.nonEmpty
            if (valid)
              sessionRef.set(Some(session)) *>
                IO(log.info("Successfully logged in to Garmin Connect!")) *>
                IO.pure(Right(session): Either[String, GarminSession])
            else
              IO.pure(
                Left("Login was not successful, check your username and password and try again.")
              )
          }.handleError { ex =>
            log.error(s"Login error: ${ex.getMessage}")
            Left(
              "Attempt to log in to Garmin Connect was not successful (this could be a server error)."
            )
          }
      }
    }

  // -------------------------------------------------------------------------
  // OAuth1 HMAC-SHA1 signing
  // -------------------------------------------------------------------------

  private def oauth1Header(
      method: String,
      url: String,
      consumerKey: String,
      consumerSecret: String,
      tokenKey: String,
      tokenSecret: String,
      extraParams: Map[String, String] = Map.empty
  ): String = {
    def pct(s: String): String =
      URLEncoder.encode(s, "UTF-8").replace("+", "%20").replace("*", "%2A").replace("%7E", "~")

    val nonce = UUID.randomUUID().toString.replace("-", "")
    val timestamp = (System.currentTimeMillis() / 1000).toString

    val oauthParams: Map[String, String] = Map(
      "oauth_consumer_key" -> consumerKey,
      "oauth_nonce" -> nonce,
      "oauth_signature_method" -> "HMAC-SHA1",
      "oauth_timestamp" -> timestamp,
      "oauth_version" -> "1.0"
    ) ++ (if (tokenKey.nonEmpty) Map("oauth_token" -> tokenKey) else Map.empty)

    val paramString = (oauthParams ++ extraParams).toSeq
      .sortBy(_._1)
      .map { case (k, v) => s"${pct(k)}=${pct(v)}" }
      .mkString("&")

    val base = s"${method.toUpperCase}&${pct(url)}&${pct(paramString)}"
    val signingKey = s"${pct(consumerSecret)}&${pct(tokenSecret)}"

    val mac = Mac.getInstance("HmacSHA1")
    mac.init(new SecretKeySpec(signingKey.getBytes("UTF-8"), "HmacSHA1"))
    val sig = Base64.getEncoder.encodeToString(mac.doFinal(base.getBytes("UTF-8")))

    "OAuth " + (oauthParams + ("oauth_signature" -> sig)).toSeq
      .sortBy(_._1)
      .map { case (k, v) => s"""$k="${pct(v)}"""" }
      .mkString(", ")
  }

  // -------------------------------------------------------------------------
  // New 5-step login flow
  // -------------------------------------------------------------------------

  private def performLogin: IO[GarminSession] = {

    val CSRF_RE = """name="_csrf"\s+value="(.+?)"""".r
    val TICKET_RE = """embed\?ticket=([^"]+)"""".r
    val TITLE_RE = """<title>(.+?)</title>""".r

    val ssoBase = "https://sso.garmin.com/sso"
    val ssoEmbed = s"$ssoBase/embed"
    val ssoUA = "GCM-iOS-5.7.2.1"
    val androidUA = "com.garmin.android.apps.connectmobile"

    def buildQS(params: Map[String, String]): String =
      params.map { case (k, v) =>
        s"${URLEncoder.encode(k, "UTF-8")}=${URLEncoder.encode(v, "UTF-8")}"
      }.mkString("&")

    val embedParams: Map[String, String] = Map(
      "id" -> "gauth-widget",
      "embedWidget" -> "true",
      "gauthHost" -> ssoBase
    )
    val signinParams: Map[String, String] = Map(
      "id" -> "gauth-widget",
      "embedWidget" -> "true",
      "gauthHost" -> ssoEmbed,
      "service" -> ssoEmbed,
      "source" -> ssoEmbed,
      "redirectAfterAccountLoginUrl" -> ssoEmbed,
      "redirectAfterAccountCreationUrl" -> ssoEmbed
    )

    def mergeCookies(jar: Map[String, String], resp: Response[IO]): Map[String, String] =
      jar ++ resp.cookies.map(rc => rc.name -> rc.content)

    def cookieHeader(jar: Map[String, String]): Header.Raw =
      Header.Raw(CIString("Cookie"), jar.map { case (k, v) => s"$k=$v" }.mkString("; "))

    // Step 1: GET /sso/embed — set initial cookies
    val step1: IO[Map[String, String]] = client
      .run(
        Request[IO](
          method = Method.GET,
          uri = Uri.unsafeFromString(s"$ssoEmbed?${buildQS(embedParams)}")
        ).withHeaders(Header.Raw(CIString("User-Agent"), ssoUA))
      )
      .use(resp => IO.pure(mergeCookies(Map.empty, resp)))

    // Step 2: GET /sso/signin — extract CSRF token from HTML
    def step2(jar: Map[String, String]): IO[(String, Map[String, String])] = {
      val signinUrl = s"$ssoBase/signin?${buildQS(signinParams)}"
      client
        .run(
          Request[IO](
            method = Method.GET,
            uri = Uri.unsafeFromString(signinUrl)
          ).withHeaders(
            Header.Raw(CIString("User-Agent"), ssoUA),
            cookieHeader(jar),
            Header.Raw(CIString("Referer"), s"$ssoEmbed?${buildQS(embedParams)}")
          )
        )
        .use { resp =>
          resp.as[String].flatMap { html =>
            val newJar = mergeCookies(jar, resp)
            CSRF_RE.findFirstMatchIn(html).map(_.group(1)) match {
              case Some(csrf) => IO.pure((csrf, newJar))
              case None =>
                IO.raiseError(
                  new RuntimeException(
                    s"Cannot find CSRF token in SSO signin page. Check account or try again later. HTML: ${html.take(400)}"
                  )
                )
            }
          }
        }
    }

    // Step 3: POST /sso/signin — submit credentials, extract ticket from HTML
    def step3(csrf: String, jar: Map[String, String]): IO[String] = {
      val signinUrl = s"$ssoBase/signin?${buildQS(signinParams)}"
      val formBody = buildQS(
        Map("username" -> email, "password" -> password, "embed" -> "true", "_csrf" -> csrf)
      )
      client
        .run(
          Request[IO](
            method = Method.POST,
            uri = Uri.unsafeFromString(signinUrl)
          ).withEntity(formBody)
            .withHeaders(
              Header.Raw(CIString("Content-Type"), "application/x-www-form-urlencoded"),
              Header.Raw(CIString("User-Agent"), ssoUA),
              cookieHeader(jar),
              Header.Raw(CIString("Referer"), signinUrl)
            )
        )
        .use { resp =>
          resp.as[String].flatMap { html =>
            TICKET_RE.findFirstMatchIn(html).map(_.group(1)) match {
              case Some(ticket) => IO.pure(ticket)
              case None =>
                if (resp.status.code == 429)
                  IO.raiseError(
                    new RuntimeException(
                      "Rate limited by Garmin SSO (429). Please wait a few minutes and try again."
                    )
                  )
                else if (html.contains("ACCOUNT_LOCKED"))
                  IO.raiseError(
                    new RuntimeException(
                      "Garmin account is locked. Too many failed login attempts. Unlock it at connect.garmin.com and try again."
                    )
                  )
                else {
                  val title = TITLE_RE.findFirstMatchIn(html).map(_.group(1)).getOrElse("unknown")
                  if (title.contains("MFA"))
                    IO.raiseError(
                      new RuntimeException(
                        "MFA is required but not supported. Disable 2FA on your Garmin account."
                      )
                    )
                  else
                    IO.raiseError(
                      new RuntimeException(
                        s"Login failed. Check your email/password. (page title: $title)"
                      )
                    )
                }
            }
          }
        }
    }

    // Step 4: Fetch OAuth1 consumer credentials from garth's public S3 bucket
    case class OAuthConsumer(key: String, secret: String)
    val step4: IO[OAuthConsumer] =
      client
        .run(
          Request[IO](
            Method.GET,
            Uri.unsafeFromString("https://thegarth.s3.amazonaws.com/oauth_consumer.json")
          )
        )
        .use { resp =>
          resp.as[String].flatMap { body =>
            parseJson(body) match {
              case Right(json) =>
                (
                  json.hcursor.get[String]("consumer_key"),
                  json.hcursor.get[String]("consumer_secret")
                ) match {
                  case (Right(k), Right(s)) => IO.pure(OAuthConsumer(k, s))
                  case _ =>
                    IO.raiseError(new RuntimeException(s"Cannot parse oauth_consumer.json: $body"))
                }
              case Left(e) =>
                IO.raiseError(new RuntimeException(s"Cannot fetch OAuth consumer credentials: $e"))
            }
          }
        }

    // Step 5: OAuth1-signed GET → exchange ticket for OAuth1 token
    val preauthorizedUrl = s"$oauthApiBase/preauthorized"

    def step5(ticket: String, consumer: OAuthConsumer): IO[(String, String)] = {
      val queryParams = Map(
        "ticket" -> ticket,
        "login-url" -> ssoEmbed,
        "accepts-mfa-tokens" -> "true"
      )
      val authHeader = oauth1Header(
        method = "GET",
        url = preauthorizedUrl,
        consumerKey = consumer.key,
        consumerSecret = consumer.secret,
        tokenKey = "",
        tokenSecret = "",
        extraParams = queryParams
      )
      client
        .run(
          Request[IO](
            method = Method.GET,
            uri = Uri.unsafeFromString(preauthorizedUrl).withQueryParams(queryParams)
          ).withHeaders(
            Header.Raw(CIString("Authorization"), authHeader),
            Header.Raw(CIString("User-Agent"), androidUA)
          )
        )
        .use { resp =>
          resp.as[String].flatMap { body =>
            val params = body
              .split("&")
              .flatMap { pair =>
                pair.split("=", 2) match {
                  case Array(k, v) => Some(k -> URLDecoder.decode(v, "UTF-8"))
                  case _ => None
                }
              }
              .toMap
            (params.get("oauth_token"), params.get("oauth_token_secret")) match {
              case (Some(t), Some(s)) => IO.pure((t, s))
              case _ =>
                IO.raiseError(
                  new RuntimeException(s"Cannot parse OAuth1 token response. Body: $body")
                )
            }
          }
        }
    }

    // Step 6: OAuth1-signed POST → exchange OAuth1 for OAuth2 Bearer (empty body)
    val exchangeUrl = s"$oauthApiBase/exchange/user/2.0"

    def step6(
        oauth1Token: String,
        oauth1Secret: String,
        consumer: OAuthConsumer
    ): IO[GarminSession] = {
      val authHeader = oauth1Header(
        method = "POST",
        url = exchangeUrl,
        consumerKey = consumer.key,
        consumerSecret = consumer.secret,
        tokenKey = oauth1Token,
        tokenSecret = oauth1Secret,
        extraParams = Map.empty
      )
      client
        .run(
          Request[IO](Method.POST, Uri.unsafeFromString(exchangeUrl)).withHeaders(
            Header.Raw(CIString("Content-Type"), "application/x-www-form-urlencoded"),
            Header.Raw(CIString("Authorization"), authHeader),
            Header.Raw(CIString("User-Agent"), androidUA)
          )
        )
        .use { resp =>
          resp.as[String].flatMap { body =>
            parseJson(body).flatMap(_.hcursor.get[String]("access_token")) match {
              case Right(token) => IO.pure(GarminSession(token))
              case Left(e) =>
                IO.raiseError(
                  new RuntimeException(s"Cannot parse OAuth2 token response: $e. Body: $body")
                )
            }
          }
        }
    }

    for {
      jar <- step1
      (csrf, jar2) <- step2(jar)
      ticket <- step3(csrf, jar2)
      consumer <- step4
      (tok, sec) <- step5(ticket, consumer)
      session <- step6(tok, sec, consumer)
    } yield session
  }

  private def sessionHeaders(session: GarminSession): List[Header.Raw] =
    List(
      Header.Raw(CIString("Authorization"), s"Bearer ${session.accessToken}"),
      Header.Raw(CIString("User-Agent"), "GCM-iOS-5.22.1.4")
    )
}

object GarminConnect {
  def make(email: String, password: String, client: Client[IO]): IO[GarminConnect] =
    for {
      ref <- Ref.of[IO, Option[GarminSession]](None)
      mutex <- Mutex[IO]
    } yield new GarminConnect(email, password, client, ref, mutex)
}
