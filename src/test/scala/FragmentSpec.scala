package io.prismic

import _root_.io.prismic.Fragment.{Timestamp, StructuredText}
import _root_.io.prismic.Fragment.StructuredText.{Block, Element, Span}
import org.joda.time.{DateTimeZone, DateTime}
import org.specs2.mutable._
import play.api.libs.json._

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
  "GeoPoint" should {
    val api = await(Api.get("https://test-public.prismic.io/api"))
    def query(q: String) = await(api.forms("everything").ref(api.master).query(q).submit())
    val doc = query("""[[:d = at(document.id, "U9pZMDQAADEAYj_n")]]""").results.head
    "get latitude & longitude" in {
      doc getGeoPoint "product.location" must beSome.like {
        case p: Fragment.GeoPoint =>
          p.latitude must_== 48.87687670000001
          p.longitude must_== 2.3338801999999825
          p.asHtml must_== """<div class="geopoint"><span class="latitude">48.87687670000001</span><span class="longitude">2.3338801999999825</span></div>"""
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
  "Timestamp" should {
    val json = Json.obj(
      "type" -> "Timestamp",
      "value" -> "2014-06-18T15:30:00+0000"
    )
    "be parsed correctly" in {
        val reference = new DateTime(2014, 6, 18, 15, 30, DateTimeZone.UTC)
        Document.parse(json) must beSome.like {
          case Timestamp(dt) => dt must_== reference
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
    "serialize to html" in {
      struct must beSome.like { case body: StructuredText =>
        body.asHtml(resolver) mustEqual
        """<h1>The end of a chapter the beginning of a new one</h1>
          |
          |<p><img alt="" src="https://prismic-io.s3.amazonaws.com/lesbonneschoses/8181933ff2f5032daff7d732e33a3beb6f57e09f.jpg" width="640" height="960" /></p>
          |
          |<p>Jean-Michel Pastranova, the founder of <em>Les Bonnes Choses</em>, and creator of the whole concept of modern fine pastry, has decided to step down as the CEO and the Director of Workshops of <em>Les Bonnes Choses</em>, to focus on other projects, among which his now best-selling pastry cook books, but also to take on a primary role in a culinary television show to be announced later this year.</p>
          |
          |<p>"I believe I've taken the <em>Les Bonnes Choses</em> concept as far as it can go. <em>Les Bonnes Choses</em> is already an entity that is driven by its people, thanks to a strong internal culture, so I don't feel like they need me as much as they used to. I'm sure they are greater ways to come, to innovate in pastry, and I'm sure <em>Les Bonnes Choses</em>'s coming innovation will be even more mind-blowing than if I had stayed longer."</p>
          |
          |<p>He will remain as a senior advisor to the board, and to the workshop artists, as his daughter Selena, who has been working with him for several years, will fulfill the CEO role from now on.</p>
          |
          |<p>"My father was able not only to create a revolutionary concept, but also a company culture that puts everyone in charge of driving the company's innovation and quality. That gives us years, maybe decades of revolutionary ideas to come, and there's still a long, wonderful path to walk in the fine pastry world."</p>"""
          .stripMargin
      }
    }
    "serialize with a custom serializer" in {
       struct must beSome.like { case body: StructuredText =>
       body.asHtml(resolver, HtmlSerializer {
          case (StructuredText.Block.Image(view, _, _), _) => s"${view.asHtml}"
          case (em: Span.Em, content) => s"<em class='italic'>$content</em>"
        }) mustEqual
        """<h1>The end of a chapter the beginning of a new one</h1>
          |
          |<p><img alt="" src="https://prismic-io.s3.amazonaws.com/lesbonneschoses/8181933ff2f5032daff7d732e33a3beb6f57e09f.jpg" width="640" height="960" /></p>
          |
          |<p>Jean-Michel Pastranova, the founder of <em class='italic'>Les Bonnes Choses</em>, and creator of the whole concept of modern fine pastry, has decided to step down as the CEO and the Director of Workshops of <em class='italic'>Les Bonnes Choses</em>, to focus on other projects, among which his now best-selling pastry cook books, but also to take on a primary role in a culinary television show to be announced later this year.</p>
          |
          |<p>"I believe I've taken the <em class='italic'>Les Bonnes Choses</em> concept as far as it can go. <em class='italic'>Les Bonnes Choses</em> is already an entity that is driven by its people, thanks to a strong internal culture, so I don't feel like they need me as much as they used to. I'm sure they are greater ways to come, to innovate in pastry, and I'm sure <em class='italic'>Les Bonnes Choses</em>'s coming innovation will be even more mind-blowing than if I had stayed longer."</p>
          |
          |<p>He will remain as a senior advisor to the board, and to the workshop artists, as his daughter Selena, who has been working with him for several years, will fulfill the CEO role from now on.</p>
          |
          |<p>"My father was able not only to create a revolutionary concept, but also a company culture that puts everyone in charge of driving the company's innovation and quality. That gives us years, maybe decades of revolutionary ideas to come, and there's still a long, wonderful path to walk in the fine pastry world."</p>"""
          .stripMargin
      }
    }
  }

  "Nested spans" should {
    val text = "abcdefghijklmnopqrstuvwxyz"
    "correctly serialize with the same starting point" in {
      Block.asHtml(Block.Paragraph(
        text,
        Seq(
          Span.Em(2, 6, None),
          Span.Strong(2, 4, None)
        ), None), resolver) mustEqual "<p>ab<em><strong>cd</strong>ef</em>ghijklmnopqrstuvwxyz</p>"
    }
    "correctly serialize with the same starting point (2)" in {
      Block.asHtml(Block.Paragraph(
        text,
        Seq(
          Span.Em(2, 4, None),
          Span.Strong(2, 6, None)
        ), None), resolver) mustEqual "<p>ab<strong><em>cd</em>ef</strong>ghijklmnopqrstuvwxyz</p>"
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
