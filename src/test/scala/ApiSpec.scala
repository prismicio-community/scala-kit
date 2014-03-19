package io.prismic

import org.specs2.mutable._

import scala.concurrent.duration._
import scala.concurrent.{ Future, Await }

class ApiSpec extends Specification {

  private def await[A](fua: Future[A]) = Await.result(fua, DurationInt(2).seconds)

  "private API without authorization" should {
    "without token" in {
      await {
        Api.get("https://private-test.prismic.io/api")
      } must throwA[ApiError].like {
        case AuthorizationNeeded(_, url) => url must_== "https://private-test.prismic.io/auth"
      }
    }
    "with an invalid token" in {
      await {
        Api.get("https://private-test.prismic.io/api", Some("dummy-token"))
      } must throwA[ApiError].like {
        case InvalidToken(_, url) => url must_== "https://private-test.prismic.io/auth"
      }
    }
  }
}
