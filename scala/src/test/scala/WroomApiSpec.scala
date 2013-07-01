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

    "getApi Chunks HTML" in {
      val maybeApi = Wroom.url("http://twelvesouth.wroom.dev").api
      val res = Await.result(
        maybeApi flatMap { api =>
          val docs = api.form("documents").withRef(api.master)
                        .withFields("types" -> Seq("product"))
                        //.withFields("tagged" -> Seq("product"))
                        .withChunks("_" -> Render.HTML)

          docs.submit()
        },
        duration.Duration("2 seconds")
      )
      println("res:"+res)

      success
    }

    "getApi Chunks JSON" in {
      val maybeApi = Wroom.url("http://twelvesouth.wroom.dev").api
      val res = Await.result(
        maybeApi flatMap { api =>
          val docs = api.form("documents")
                        .withRef(api.master)
                        .withFields("types" -> Seq("product"))
          docs.submit()
        },
        duration.Duration("2 seconds")
      )
      println("res:"+res)

      success
    }
/*
    "different syntaxes" in {
      Await.result(
        (for{
          api   <- Wroom.url("http://twelvesouth.wroom.dev").api()
          master = api.master
          docs1 <- master.form("documents")
                         .withFields("tagged" -> Seq("product"))
                         .withChunks("product.name" -> Render.HTML)
                         .submit
          docs2 <- master.form("documents")
                         .withFields("tagged" -> Seq("product"))
                         .withChunks("product.name")
                         .submit
          docs3 <- master.form("documents")
                         .withFields(FieldFilter("tagged", Seq("product")))
                         .withChunks(
                           ChunkFilter("product.name", Render.HTML).withProps("field1" -> "value1", "field2" -> "value2")
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
          api   <- Wroom.url("http://twelvesouth.wroom.dev")
                        .withQueryString("access_token" -> "d711cadd469d0767422bf4d314cc561a")
                        .api()
          master = api.master
          docs1 <- master.form("document")
                         .withFields(
                           "id" -> "AAAAAAdbzTgAAAAA"/*,
                           "ref" -> "AAAAAAdbzT8AAAAA"*/
                         )
                         .withChunks("article.body" -> Render.HTML)
                         .submit
        } yield(docs1)).map{ docs1 =>
          println("docs1:"+docs1)
          success
        },
        duration.Duration("2 seconds")
      )
    }
*/
  }
}
