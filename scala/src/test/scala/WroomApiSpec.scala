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
          val docs = master.form("documents").withTags(
            "tagged" -> Seq("product")
          )

          docs.submit()
        },
        duration.Duration("2 seconds")
      )
      println("res:"+Json.prettyPrint(res.json))
      success
    }
  }
}
