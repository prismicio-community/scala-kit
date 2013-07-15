import org.specs2.mutable._

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import scala.concurrent._
import ExecutionContext.Implicits.global

import play.api.libs.json._

import com.zenexity.wroom.client._

@RunWith(classOf[JUnitRunner])
class WroomApiSpec extends Specification {

  "WroomApi" should {
    "getApi" in {
      Wroom.url("http://twelvesouth.wroom.dev").api.foreach( println )
      success
    }

    "getApi Article" in {
      val maybeApi = Wroom.url("http://twelvesouth.wroom.dev").api
      val articles = Await.result(
        maybeApi flatMap { api =>
          val q = api.form("documents")
                     .q("""[[ $d document.type "article"]]""")

          q.ref(api.master).submit
        },
        duration.Duration("2 seconds")
      )
      println("articles:"+articles)

      println("\n\narticle HTML:"+articles.head.fragments.map{ case (k, v) => k -> v.asHtml })

      success
    }

  }
}
