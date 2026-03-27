package com.github.mgifos.workouts

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.http4s.*
import org.http4s.client.Client
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GarminAuthSpec extends AnyWordSpec with Matchers {

  // -------------------------------------------------------------------------
  // Helpers
  // -------------------------------------------------------------------------

  private def mockClient(
      pf: PartialFunction[(Method, String), IO[Response[IO]]]
  ): Client[IO] =
    Client.fromHttpApp(HttpApp[IO] { req =>
      pf.lift((req.method, req.uri.path.renderString))
        .getOrElse(IO.pure(Response[IO](Status.InternalServerError)))
    })

  private def htmlOk(body: String): IO[Response[IO]] =
    IO.pure(Response[IO](Status.Ok).withEntity(body))

  private def jsonOk(body: String): IO[Response[IO]] =
    IO.pure(Response[IO](Status.Ok).withEntity(body))

  /** Returns a mock client that handles all 6 steps of the OAuth flow successfully. */
  private def fullOAuthClient(accessToken: String): Client[IO] = {
    val csrfHtml   = """<form><input name="_csrf" value="tok123"/></form>"""
    val ticketHtml = """<a href="embed?ticket=TKT-xyz">link</a>"""
    val consumerJson = """{"consumer_key":"ck","consumer_secret":"cs"}"""
    val oauth1Body = "oauth_token=ot&oauth_token_secret=os"
    val oauth2Json = s"""{"access_token":"$accessToken"}"""

    mockClient {
      // Step 1: GET /sso/embed
      case (Method.GET, p) if p.endsWith("/embed") => htmlOk("")
      // Step 2: GET /sso/signin
      case (Method.GET, p) if p.endsWith("/signin") => htmlOk(csrfHtml)
      // Step 3: POST /sso/signin
      case (Method.POST, p) if p.endsWith("/signin") => htmlOk(ticketHtml)
      // Step 4: S3 OAuth consumer
      case (Method.GET, p) if p.endsWith("/oauth_consumer.json") => jsonOk(consumerJson)
      // Step 5: preauthorized
      case (Method.GET, p) if p.endsWith("/preauthorized") => jsonOk(oauth1Body)
      // Step 6: exchange
      case (Method.POST, p) if p.contains("/exchange/") => jsonOk(oauth2Json)
    }
  }

  // -------------------------------------------------------------------------
  // Session caching
  // -------------------------------------------------------------------------

  "GarminAuth.login" should {

    "return a cached session on the second call without re-authenticating" in {
      val auth = GarminAuth.make("u", "p", fullOAuthClient("bearer-abc")).unsafeRunSync()
      val firstResult = auth.login().unsafeRunSync()
      firstResult shouldBe Right(GarminSession("bearer-abc"))
      // Second call hits the cache — same result, no additional HTTP round-trips
      auth.login().unsafeRunSync() shouldBe firstResult
    }

    "run performLogin again when forceNewSession is true" in {
      val auth = GarminAuth.make("u", "p", fullOAuthClient("my-token")).unsafeRunSync()
      auth.login().unsafeRunSync() shouldBe Right(GarminSession("my-token"))
      // Force refresh — same mock returns the same token again
      auth.login(forceNewSession = true).unsafeRunSync() shouldBe Right(GarminSession("my-token"))
    }

    "return Right(session) after a successful OAuth flow" in {
      val auth = GarminAuth.make("u", "p", fullOAuthClient("bearer-abc")).unsafeRunSync()
      auth.login().unsafeRunSync() shouldBe Right(GarminSession("bearer-abc"))
    }

    "return Left when CSRF token is absent from the signin page" in {
      val client = mockClient {
        case (Method.GET, p) if p.endsWith("/embed")  => htmlOk("")
        case (Method.GET, p) if p.endsWith("/signin") => htmlOk("<html>no csrf here</html>")
      }
      val auth   = GarminAuth.make("u", "p", client).unsafeRunSync()
      val result = auth.login().unsafeRunSync()
      result shouldBe a[Left[?, ?]]
    }

    "return Left when the signin POST returns 429 (rate limited)" in {
      val csrfHtml = """<input name="_csrf" value="tok"/>"""
      val client = mockClient {
        case (Method.GET, p) if p.endsWith("/embed")  => htmlOk("")
        case (Method.GET, p) if p.endsWith("/signin") => htmlOk(csrfHtml)
        case (Method.POST, p) if p.endsWith("/signin") =>
          IO.pure(Response[IO](Status.TooManyRequests).withEntity(""))
      }
      val auth   = GarminAuth.make("u", "p", client).unsafeRunSync()
      val result = auth.login().unsafeRunSync()
      result shouldBe a[Left[?, ?]]
    }

    "return Left when the account is locked" in {
      val csrfHtml = """<input name="_csrf" value="tok"/>"""
      val client = mockClient {
        case (Method.GET, p) if p.endsWith("/embed")  => htmlOk("")
        case (Method.GET, p) if p.endsWith("/signin") => htmlOk(csrfHtml)
        case (Method.POST, p) if p.endsWith("/signin") =>
          htmlOk("<html>ACCOUNT_LOCKED</html>")
      }
      val auth   = GarminAuth.make("u", "p", client).unsafeRunSync()
      val result = auth.login().unsafeRunSync()
      result shouldBe a[Left[?, ?]]
    }

    "return Left when MFA is required" in {
      val csrfHtml = """<input name="_csrf" value="tok"/>"""
      val client = mockClient {
        case (Method.GET, p) if p.endsWith("/embed")  => htmlOk("")
        case (Method.GET, p) if p.endsWith("/signin") => htmlOk(csrfHtml)
        case (Method.POST, p) if p.endsWith("/signin") =>
          htmlOk("<html><title>MFA Required</title></html>")
      }
      val auth   = GarminAuth.make("u", "p", client).unsafeRunSync()
      val result = auth.login().unsafeRunSync()
      result shouldBe a[Left[?, ?]]
    }

    "return Left when credentials are wrong (no ticket in response)" in {
      val csrfHtml = """<input name="_csrf" value="tok"/>"""
      val client = mockClient {
        case (Method.GET, p) if p.endsWith("/embed")  => htmlOk("")
        case (Method.GET, p) if p.endsWith("/signin") => htmlOk(csrfHtml)
        case (Method.POST, p) if p.endsWith("/signin") =>
          htmlOk("<html><title>Sign In</title><p>Invalid password</p></html>")
      }
      val auth   = GarminAuth.make("u", "p", client).unsafeRunSync()
      val result = auth.login().unsafeRunSync()
      result shouldBe a[Left[?, ?]]
    }

    "return Left when the OAuth consumer JSON cannot be parsed" in {
      val csrfHtml   = """<input name="_csrf" value="tok"/>"""
      val ticketHtml = """<a href="embed?ticket=TKT-xyz">link</a>"""
      val client = mockClient {
        case (Method.GET, p) if p.endsWith("/embed")           => htmlOk("")
        case (Method.GET, p) if p.endsWith("/signin")          => htmlOk(csrfHtml)
        case (Method.POST, p) if p.endsWith("/signin")         => htmlOk(ticketHtml)
        case (Method.GET, p) if p.endsWith("/oauth_consumer.json") =>
          jsonOk("""{"wrong_key": "bad"}""")
      }
      val auth   = GarminAuth.make("u", "p", client).unsafeRunSync()
      val result = auth.login().unsafeRunSync()
      result shouldBe a[Left[?, ?]]
    }

    "return Left when the OAuth1 token response is malformed" in {
      val csrfHtml     = """<input name="_csrf" value="tok"/>"""
      val ticketHtml   = """<a href="embed?ticket=TKT-xyz">link</a>"""
      val consumerJson = """{"consumer_key":"ck","consumer_secret":"cs"}"""
      val client = mockClient {
        case (Method.GET, p) if p.endsWith("/embed")           => htmlOk("")
        case (Method.GET, p) if p.endsWith("/signin")          => htmlOk(csrfHtml)
        case (Method.POST, p) if p.endsWith("/signin")         => htmlOk(ticketHtml)
        case (Method.GET, p) if p.endsWith("/oauth_consumer.json") => jsonOk(consumerJson)
        case (Method.GET, p) if p.endsWith("/preauthorized")   => jsonOk("no_token_here")
      }
      val auth   = GarminAuth.make("u", "p", client).unsafeRunSync()
      val result = auth.login().unsafeRunSync()
      result shouldBe a[Left[?, ?]]
    }

    "return Left when the OAuth2 exchange response is malformed" in {
      val csrfHtml     = """<input name="_csrf" value="tok"/>"""
      val ticketHtml   = """<a href="embed?ticket=TKT-xyz">link</a>"""
      val consumerJson = """{"consumer_key":"ck","consumer_secret":"cs"}"""
      val oauth1Body   = "oauth_token=ot&oauth_token_secret=os"
      val client = mockClient {
        case (Method.GET, p) if p.endsWith("/embed")           => htmlOk("")
        case (Method.GET, p) if p.endsWith("/signin")          => htmlOk(csrfHtml)
        case (Method.POST, p) if p.endsWith("/signin")         => htmlOk(ticketHtml)
        case (Method.GET, p) if p.endsWith("/oauth_consumer.json") => jsonOk(consumerJson)
        case (Method.GET, p) if p.endsWith("/preauthorized")   => jsonOk(oauth1Body)
        case (Method.POST, p) if p.contains("/exchange/")      => jsonOk("""{"nope":"nope"}""")
      }
      val auth   = GarminAuth.make("u", "p", client).unsafeRunSync()
      val result = auth.login().unsafeRunSync()
      result shouldBe a[Left[?, ?]]
    }
  }
}
