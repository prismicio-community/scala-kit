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
    "private" in {
      await {
        // startgist:56fb341dba38843df8d4:prismic-apiPrivate.scala
        // This will fail because the token is invalid, but this is how to access a private API
        val apiFuture = Api.get("https://lesbonneschoses.prismic.io/api", Some("MC5-XXXXXXX-vRfvv70"))
        // endgist
        apiFuture
      } must throwAn[Exception]
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

    "StructuredText.asHtml" in {
      val h = await {
        Api.get("https://lesbonneschoses.prismic.io/api").flatMap { api =>
          api.forms("everything")
            .ref(api.master)
            .query(Predicate.at("document.id", "UlfoxUnM0wkXYXbX"))
            .submit()
            .map { response: Response =>
// startgist:7da680aff5aaf5e61ba5:prismic-asHtml.scala
            val doc = response.results.head
            val resolver = DocumentLinkResolver { link =>
              s"/testing_url/${link.id}/${link.slug}"
            }
            val html = doc.getStructuredText("blog-post.body").map(_.asHtml(resolver))
// endgist
            html
          }
        }
      }
      h must beSome.like {
        case s: String => s mustEqual
          """<h1>Get the right approach to ganache</h1>
            |
            |<p>A lot of people touch base with us to know about one of our key ingredients, and the essential role it plays in our creations: ganache.</p>
            |
            |<p>Indeed, ganache is the macaron's softener, or else, macarons would be but tough biscuits; it is the cupcake's wrapper, or else, cupcakes would be but plain old cake. We even sometimes use ganache within our cupcakes, to soften the cake itself, or as a support to our pies' content.</p>
            |
            |<h2>How to approach ganache</h2>
            |
            |<p class="block-img"><img alt="" src="https://prismic-io.s3.amazonaws.com/lesbonneschoses/ee7b984b98db4516aba2eabd54ab498293913c6c.jpg" width="640" height="425" /></p>
            |
            |<p>Apart from the taste balance, which is always a challenge when it comes to pastry, the tough part about ganache is about thickness. It is even harder to predict through all the phases the ganache gets to meet (how long will it get melted? how long will it remain in the fridge?). Things get a hell of a lot easier to get once you consider that there are two main ways to get the perfect ganache:</p>
            |
            |<ul>
            |
            |<li><strong>working from the top down</strong>: start with a thick, almost hard material, and soften it by manipulating it, or by mixing it with a more liquid ingredient (like milk)</li>
            |
            |<li><strong>working from the bottom up</strong>: start from a liquid-ish state, and harden it by miwing it with thicker ingredients, or by leaving it in the fridge longer.</li>
            |
            |</ul>
            |
            |<p>We do hope this advice will empower you in your ganache-making skills. Let us know how you did with it!</p>
            |
            |<h2>Ganache at <em>Les Bonnes Choses</em></h2>
            |
            |<p>We have a saying at Les Bonnes Choses: "Once you can make ganache, you can make anything."</p>
            |
            |<p>As you may know, we like to give our workshop artists the ability to master their art to the top; that is why our Preparation Experts always start off as being Ganache Specialists for Les Bonnes Choses. That way, they're given an opportunity to focus on one exercise before moving on. Once they master their ganache, and are able to provide the most optimal delight to our customers, we consider they'll thrive as they work on other kinds of preparations.</p>
            |
            |<h2>About the chocolate in our ganache</h2>
            |
            |<p>Now, we've also had a lot of questions about how our chocolate gets made. It's true, as you might know, that we make it ourselves, from Columbian cocoa and French cow milk, with a process that much resembles the one in the following Discovery Channel documentary.</p>
            |
            |<div data-oembed="http://www.youtube.com/watch?v=Ye78F3-CuXY" data-oembed-type="video" data-oembed-provider="youtube"><iframe width="459" height="344" src="http://www.youtube.com/embed/Ye78F3-CuXY?feature=oembed" frameborder="0" allowfullscreen></iframe></div>""".stripMargin
      }
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
              // Add a class to em tags
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
            |<p class="block-img"><img alt="" src="https://prismic-io.s3.amazonaws.com/lesbonneschoses/8181933ff2f5032daff7d732e33a3beb6f57e09f.jpg" width="640" height="960" /></p>
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
