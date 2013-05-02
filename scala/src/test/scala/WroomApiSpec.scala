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
      println("res:"+res)

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
                         .withFields(FieldFilter("tagged", Seq("product")))
                         .withWidgets(
                           WidgetFilter("product.name", Render.HTML).withProps("field1" -> "value1", "field2" -> "value2")
                         )
                         .submit
        } yield(docs1, docs2, docs3)).map{ case (docs1, docs2, docs3) =>
          docs1 must beEqualTo(docs2)
          docs1 must beEqualTo(docs3)
        },
        duration.Duration("2 seconds")
      )
    }


    "calling document" in {
      Await.result(
        (for{
          api   <- Wroom.getApi("http://twelvesouth.wroom.dev")
          master = api.master
          docs1 <- master.form("document")
                         .withFields(
                           "id" -> "AAAAAAdbzTgAAAAA",
                           "ref" -> "AAAAAAdbzT8AAAAA"
                         )
                         .withWidgets("article.body" -> Render.HTML)
                         .submit
        } yield(docs1)).map{ docs1 =>
          println("docs1:"+docs1)
          success
        },
        duration.Duration("2 seconds")
      )
    }    
  }
}
