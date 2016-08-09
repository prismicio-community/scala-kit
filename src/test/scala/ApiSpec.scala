package io.prismic

import io.prismic.Fragment.DocumentLink
import org.specs2.mutable._

import scala.concurrent.duration._
import scala.concurrent.{ Future, Await }

class ApiSpec extends Specification {

  private def await[A](fua: Future[A]) = Await.result(fua, DurationInt(5).seconds)

//  def lbc = Api.get("https://lesbonneschoses.cdn.prismic.io/api")

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
/*
  "queries on the repo" should {
    "with ordering" in {
      val api = await(lbc)
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

  "Fetch additional fields in links" should {
    "with fetchLinks" in {
      val api = await(lbc)
      val doc = await {
        api.forms("everything")
          .ref(api.master)
          .fetchLinks(Set("blog-post.author"))
          .query(Predicate.at("document.id", "UlfoxUnM0wkXYXbt"))
          .submit()
      }.results.head
      doc.getLink("blog-post.relatedpost") must beSome.like {
        case doclink: DocumentLink => doclink.getText("blog-post.author") must_== Some("John M. Martelle, Fine Pastry Magazine")
      }
    }
  }
 */
}
