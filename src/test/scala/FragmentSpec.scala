package io.prismic

import org.specs2.mutable._

import scala.concurrent.duration._
import scala.concurrent.{ Future, Await }

class FragmentSpec extends Specification {

  private def await[A](fua: Future[A]) = Await.result(fua, DurationInt(2).seconds)

  private lazy val api = await(Api.get("https://micro.prismic.io/api", cache = BuiltInCache()))
  private def query(q: String) = await(api.forms("everything").ref(api.master).query(q).submit())

  "Group" should {
    val docChapter = query("""[[:d = at(document.type, "docchapter")]]""").head
    "access fields" in {
      docChapter getGroup "docchapter.docs" must beSome.like {
        case group => group.docs.headOption must beSome.like {
          case doc => doc.getLink("linktodoc") must beSome.like {
            case link: Fragment.Link => success
          }
        }
      }
    }
  }
}
