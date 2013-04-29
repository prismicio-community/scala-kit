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
      val maybeApi = Wroom.getApi("http://twelvesouth.wroom.dev") 
      val res = Await.result(
        maybeApi flatMap { api =>
          val master = api.master

          val docs = master.form("documents")
                      .withFields("tagged" -> Seq("product"))
                      .withWidgets("product.name" -> Render.HTML)

          docs.submit()
        },
        duration.Duration("2 seconds")
      )
      println("res:"+Json.prettyPrint(res.json))

      success
    }

    "different syntaxes" in {
      Await.result(
        (for{
          api   <- Wroom.getApi("http://twelvesouth.wroom.dev")
          master = api.master
          docs1 <- master.form("documents")
                         .withFields("tagged" -> Seq("product"))
                         .withWidgets("product.name" -> Render.HTML)
                         .submit
          docs2 <- master.form("documents")
                         .withFields("tagged" -> Seq("product"))
                         .withWidgets("product.name")
                         .submit
          docs3 <- master.form("documents")
                         .withFields(Field("tagged", Seq("product")))
                         .withWidgets(Widget("product.name", Render.HTML))
                         .submit
        } yield(docs1, docs2, docs3)).map{ case (docs1, docs2, docs3) =>
          docs1.json must beEqualTo(docs2.json)
          docs1.json must beEqualTo(docs3.json)
        },
        duration.Duration("2 seconds")
      )
    }
  }
}
