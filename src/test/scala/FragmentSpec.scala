package io.prismic

import org.specs2.mutable._

import scala.concurrent.duration._
import scala.concurrent.{ Future, Await }

class FragmentSpec extends Specification {

  private def await[A](fua: Future[A]) = Await.result(fua, DurationInt(2).seconds)
  private def resolver = DocumentLinkResolver { link =>
    s"""http://localhost/${link.typ}/${link.id}"""
  }

  "Group" should {
    val api = await(Api.get("https://micro.prismic.io/api"))
    def query(q: String) = await(api.forms("everything").ref(api.master).query(q).submit())
    val docChapter = query("""[[:d = at(document.id, "UrDcEAEAAKUbpbND")]]""").results.head
    "access fields" in {
      docChapter getGroup "docchapter.docs" must beSome.like {
        case group => group.docs.headOption must beSome.like {
          case doc => doc.getLink("linktodoc") must beSome.like {
            case link: Fragment.Link => success
          }
        }
      }
    }
    "serialize to HTML" in {
      docChapter getGroup "docchapter.docs" must beSome.like {
        case group => group asHtml resolver must_==
          """<section data-field="linktodoc"><a href="http://localhost/doc/UrDejAEAAFwMyrW9">installing-meta-micro</a></section>
<section data-field="desc"><p>Just testing another field in a group section.</p></section>
<section data-field="linktodoc"><a href="http://localhost/doc/UrDmKgEAALwMyrXA">using-meta-micro</a></section>"""
      }
    }
  }
  "Link" should {
    val api = await(Api.get("https://test-public.prismic.io/api"))
    def query(q: String) = await(api.forms("everything").ref(api.master).query(q).submit())
    val doc = query("""[[:d = at(document.id, "Uyr9_wEAAKYARDMV")]]""").results.head
    "support media" in {
      doc getLink "test-link.related" must beSome.like {
        case l: Fragment.MediaLink => l.filename must_== "baastad.pdf"
      }
    }
  }
  "Multiple document link" should {
    val api = await(Api.get("https://lesbonneschoses.prismic.io/api"))
    def query(q: String) = await(api.forms("everything").ref(api.master).query(q).submit())
    val doc = query("""[[:d = at(document.id, "UlfoxUnM0wkXYXbs")]]""").results.head
    "find first link" in {
      doc getLink "job-offer.location" must beSome.like {
        case l: Fragment.DocumentLink => l.slug must_== "new-york-fifth-avenue"
      }
    }
    "find all links" in {
      val links = doc getAll "job-offer.location"
      links must haveSize(5)
      links lift 0 must beSome.like {
        case l: Fragment.DocumentLink => l.slug must_== "new-york-fifth-avenue"
      }
      links lift 1 must beSome.like {
        case l: Fragment.DocumentLink => l.slug must_== "tokyo-roppongi-hills"
      }
    }
  }
  "StructuredText" should {
    val api = await(Api.get("https://lesbonneschoses.prismic.io/api"))
    def query(q: String) = await(api.forms("everything").ref(api.master).query(q).submit())
    val doc = query("""[[:d = at(document.id, "UlfoxUnM0wkXYXbt")]]""").results.head
    val struct = doc getStructuredText "blog-post.body"
    "find first" in {
      struct must beSome
    }
  }
  "Image" should {
    val api = await(Api.get("https://test-public.prismic.io/api"))
    def query(q: String) = await(api.forms("everything").ref(api.master).query(q).submit())
    val doc = query("""[[:d = at(document.id, "Uyr9sgEAAGVHNoFZ")]]""").results.head
    val img = doc.getImage("article.illustration", "icon")
    val url = "https://prismic-io.s3.amazonaws.com/test-public/9f5f4e8a5d95c7259108e9cfdde953b5e60dcbb6.jpg"
    "find first" in {
      img must beSome.like {
        case v: Fragment.Image.View => v.asHtml must_== s"""<img alt="some alt text" src="$url" width="100" height="100" />"""
      }
    }
  }
}
