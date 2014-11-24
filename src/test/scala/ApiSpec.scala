package io.prismic

import org.specs2.mutable._

import scala.concurrent.duration._
import scala.concurrent.{ Future, Await }

class ApiSpec extends Specification {

  private def await[A](fua: Future[A]) = Await.result(fua, DurationInt(5).seconds)

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

  "queries on the repo" should {
    "with ordering" in {
      val api = await(Api.get("https://lesbonneschoses.prismic.io/api"))
      val orderings0 = api.forms("everything")
        .ref(api.master)
        .query(Predicate.at("document.type", "product"))
        .orderings()
        .submit()
      val orderings1 = api.forms("everything")
          .ref(api.master)
          .query(Predicate.at("document.type", "product"))
          .orderings("[my.product.price desc]")
          .submit()
      val orderings2 = api.forms("everything")
          .ref(api.master)
          .query(Predicate.at("document.type", "product"))
          .orderings("my.product.price")
          .submit()
      await(orderings0)
      val big = await(orderings1).results.head.getNumber("product.price").get.value
      val small = await(orderings2).results.head.getNumber("product.price").get.value
      big must be greaterThan small
    }
  }

}
