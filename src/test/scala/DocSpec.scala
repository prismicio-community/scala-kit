package io.prismic

import java.util.Date

import io.prismic.Fragment.StructuredText
import io.prismic.Fragment.StructuredText.Span
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

  private def resolver = DocumentLinkResolver { link =>
    s"""http://localhost/${link.typ}/${link.id}"""
  }

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

    "HTML Serializer" in {
      val h = await {
        Api.get("https://lesbonneschoses.prismic.io/api").flatMap { api =>
          api.forms("everything")
            .ref(api.master)
            .query(Predicate.at("document.id", "UlfoxUnM0wkXYXbt"))
            .submit()
            .map { response: Response =>
            val doc: Document = response.results.head
// startgist:a3924848b9b5f5d4e482:prismic-htmlSerializer.scala
            val htmlSerializer = HtmlSerializer {
              // Don't wrap images in a <p> tag
              case (StructuredText.Block.Image(view, _, _), _) => s"${view.asHtml}"
              // Add a class to hyperlinks
              case (em: Span.Em, content) => s"<em class='italic'>$content</em>"
            }
            val html = doc.getStructuredText("blog-post.body").map(_.asHtml(resolver, htmlSerializer))
// endgist
            html
          }
        }
      }
      h must beSome.like {
        case s: String => s mustEqual
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

}
