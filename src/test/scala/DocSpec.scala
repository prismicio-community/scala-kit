package io.prismic

import java.util.Date

import org.joda.time.DateTime
import org.specs2.mutable._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * Snippets for the online documentation - included here so we can compile them, run them,
 * ensure they're correct
 */
class DocSpec extends Specification {

  private def await[A](fua: Future[A]) = Await.result(fua, DurationInt(5).seconds)

  "API" should {
    "fetch" in {
      val api = await {
// startgist:f5c7c0a59790bed0b3b7:prismic-api.scala
        val apiFuture: Future[io.prismic.Api] = Api.get("https://lesbonneschoses.prismic.io/api")
        apiFuture.map { api =>
          println("References: " + api.refs)
          api
        }
// endgist
      }
      api.refs.size.mustEqual(1)
    }
    "simple query" in {
      val resp: Response = await {
// startgist:ae4378398935f89045bd:prismic-simplequery.scala
        Api.get("https://lesbonneschoses.prismic.io/api").flatMap { api =>
          api.forms("everything")
            .ref(api.master)
            .query(Predicate.at("document.type", "product")).submit().map { response =>
            // The response object contains all documents of type "product", paginated
            response
          }
        }
// endgist
      }
      resp.resultsSize.mustEqual(16)
    }

    "predicates" in {
      val resp = await {
// startgist:f1cca71970ad71a4c6ef:prismic-predicates.scala
        Api.get("https://lesbonneschoses.prismic.io/api").flatMap { api =>
          api.forms("everything").ref(api.master).query(
            Predicate.at("document.type", "blog-post"),
            Predicate.dateAfter("my.blog-post.date", new DateTime(2014, 6, 1, 0, 0))
          ).submit().map { response =>
            // All documents of type "product", updated after June 1st, 2014
            response
          }
        }
// endgist
      }
      resp.resultsSize.mustEqual(0)
    }

  }





}
