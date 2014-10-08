package io.prismic

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
  }

}
