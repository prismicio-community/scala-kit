package io.prismic

import org.specs2.mutable._

import scala.concurrent.duration._
import scala.concurrent.{ Future, Await }
import scala.concurrent.ExecutionContext.Implicits.global

class ErrorSpec extends Specification {

  private def await[A](fua: Future[A]) = Await.result(fua, DurationInt(5).seconds)

  "Query the API" should {
    "without a ref" in {
      await {
        val qs = s"""[[:id = at(my.job-offer.pouet, "ABCDEF12345")]]"""
        Api.get("https://lesbonneschoses.prismic.io/api") flatMap {
          _.forms("everything").query(qs).submit()
        }
      } must throwA[RuntimeException].like {
        case e => e.getMessage must contain("Release not provided")
      }
    }
    "with an invalid key" in {
      await {
        val qs = s"""[[:id = at(my.job-offer.pouet, "ABCDEF12345")]]"""
        Api.get("https://lesbonneschoses.prismic.io/api") flatMap { api =>
          api.forms("everything").query(qs).ref(api.master).submit()
        }
      } must throwA[RuntimeException].like {
        case e => e.getMessage must contain("unexpected field 'my.job-offer.pouet'")
      }
    }
  }
}

