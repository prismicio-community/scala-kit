package io.prismic

import io.prismic.fragments._, StructuredText.{ Block, Element, Span }
import org.joda.time.{ DateTimeZone, DateTime }
import org.specs2.mutable._
import spray.json._
import PrismicJsonProtocol._

import scala.concurrent.duration._
import scala.concurrent.{ Future, Await }

class FragmentSpec extends Specification {

  private def await[A](fua: Future[A]) = Await.result(fua, DurationInt(2).seconds)
  private def resolver = DocumentLinkResolver { link =>
    s"""http://localhost/${link.typ}/${link.id}"""
  }

  "Group" should {
    val api = await(Api.get("https://micro.prismic.io/api"))
    def query(q: Predicate) = await(api.forms("everything").ref(api.master).query(q).submit())
    val docChapter = query(Predicate.at("document.id", "UrDcEAEAAKUbpbND")).results.head
    "access fields" in {
      docChapter.slug must_== "getting-started"
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
    "not be None if a DocumentLink is broken" in {
      val json = JsonParser(
        """
          |{
          |  "id":"VFo9SDIAAC0A8Ixa",
          |  "uid":null,
          |  "type":"article",
          |  "href":"http://toto.wroom.dev/api/documents/search?ref=VFo9SDIAAC0A8Ixa&q=%5B%5B%3Ad+%3D+at%28document.id%2C+%22VQ_hV31Za5EAy02H%22%29+%5D%5D",
          |  "tags":[],
          |  "slugs":["une-activite"],
          |  "linked_documents":[],
          |  "lang": "fr-fr",
          |  "data": {
          |    "article": {
          |      "a_group": {
          |        "type": "Group",
          |        "value": [
          |          {
          |            "link": {
          |              "type": "Link.document",
          |              "value": {
          |                "document": {
          |                  "id": "UtUsygEAAEoh6CmH",
          |                  "type": "machine",
          |                  "tags": [],
          |                  "slug": "slug-1",
          |                  "lang": "en-us",
          |                  "link_type": "Document"
          |                },
          |                "isBroken": false
          |              }
          |            }
          |          },
          |          {
          |            "link": {
          |              "type": "Link.document",
          |              "value": {
          |                "document": {
          |                  "id": "UtUtIAEAAFEh6CmT",
          |                  "type": "machine",
          |                  "tags": [ ],
          |                  "slug": "-",
          |                  "lang": null,
          |                  "link_type": "Document"
          |                },
          |                "isBroken": true
          |              }
          |            }
          |          }
          |        ]
          |      }
          |    }
          |  }
          |}
        """.stripMargin)
      val doc: Option[Group] = json.convertTo[Document].getGroup("article.a_group")
      doc mustNotEqual None
      val docList = doc.get.docs
      docList.size mustEqual 2
      docList.head.getLink("link").get match {
        case DocumentLink(_, _, _, _, _, _, _, isBroken, _) => isBroken mustEqual false
      }
      docList.last.getLink("link").get match {
        case DocumentLink(_, _, _, _, _, _, _, isBroken, _) => isBroken mustEqual true
      }
    }
  }
  "GeoPoint" should {
    val api = await(Api.get("https://test-public.prismic.io/api"))
    def query(q: Predicate) = await(api.forms("everything").ref(api.master).query(q).submit())
    val doc = query(Predicate.at("document.id", "U9pZMDQAADEAYj_n")).results.head
    "get latitude & longitude" in {
      doc getGeoPoint "product.location" must beSome.like {
        case p: GeoPoint =>
          p.latitude must_== 48.87687670000001
          p.longitude must_== 2.3338801999999825
          p.asHtml must_== """<div class="geopoint"><span class="latitude">48.87687670000001</span><span class="longitude">2.3338801999999825</span></div>"""
      }
    }
  }
  "Link" should {
    lazy val document = Fixtures.document
    document.getLink("store.link") must beSome.like {
      case d: DocumentLink =>
        d.fragments.get("timestamp.text") must beSome.like {
          case s: StructuredText =>
            s.asHtml(resolver) must_== "<p>This is text</p>"
        }
    }
  }
  "Timestamp" should {
    val json = JsObject(
      "type" -> JsString("Timestamp"),
      "value" -> JsString("2014-06-18T15:30:00+0000")
    )
    "be parsed correctly" in {
        val reference = new DateTime(2014, 6, 18, 15, 30, DateTimeZone.UTC)
        json.convertTo[Fragment] match {
          case Timestamp(dt) => dt must_== reference
          case _ => ko("Timestamp expected")
        }
    }
  }

  "Slices" should {
      val json = JsonParser(
        """
          |{
          |      "id":"VQ_hV31Za5EAy02H",
          |      "uid":null,
          |      "type":"article",
          |      "href":"http://toto.wroom.dev/api/documents/search?ref=VQ_uWX1Za0oCy46m&q=%5B%5B%3Ad+%3D+at%28document.id%2C+%22VQ_hV31Za5EAy02H%22%29+%5D%5D",
          |      "tags":[],
          |      "slugs":["une-activite"],
          |      "linked_documents":[],
          |      "lang": "fr-fr",
          |       "data":{
          |           "article":{
          |               "blocks":{
          |                   "type":"SliceZone",
          |                   "value":[{
          |                       "type":"Slice",
          |                       "slice_type": "features",
          |                       "value":{
          |                           "type":"Group",
          |                           "value":[{
          |                               "illustration":{
          |                                   "type":"Image",
          |                                   "value":{
          |                                       "main":{
          |                                           "url":"https://wroomdev.s3.amazonaws.com/toto/db3775edb44f9818c54baa72bbfc8d3d6394b6ef_hsf_evilsquall.jpg",
          |                                           "alt":"",
          |                                           "copyright":"",
          |                                           "dimensions":{"width":4285,"height":709}
          |                                       },
          |                                       "views":{}
          |                                   }
          |                               },
          |                               "title":{
          |                                   "type":"Text",
          |                                   "value":"c'est un bloc features"
          |                               }
          |                           }]
          |                       }
          |                   },{
          |                       "type":"Slice",
          |                       "slice_type":"text",
          |                       "value":{
          |                           "type":"StructuredText",
          |                           "value":[{
          |                               "type":"paragraph",
          |                               "text":"C'est un bloc content",
          |                               "spans":[]
          |                           }]
          |                       }
          |                   }]
          |               }
          |           }
          |       }
          |   }
        """.stripMargin)
      val struct = json.convertTo[Document].getSliceZone("article.blocks")
      "serialize to html" in {
        struct must beSome.like { case blocks: SliceZone =>
          blocks.asHtml(resolver) mustEqual
            """<div data-slicetype="features" class="slice"><section data-field="illustration"><img alt="" src="https://wroomdev.s3.amazonaws.com/toto/db3775edb44f9818c54baa72bbfc8d3d6394b6ef_hsf_evilsquall.jpg" width="4285" height="709" /></section>
              |<section data-field="title"><span class="text">c'est un bloc features</span></section></div>
              |<div data-slicetype="text" class="slice"><p>C'est un bloc content</p></div>""".stripMargin
        }
      }


    val json2 = JsonParser(
      """
        |{
        |      "id": "Wu20hiIAANyI48lV",
        |      "uid": "page-contact",
        |      "type": "pageweb",
        |      "href": "https://t-stook.cdn.prismic.io/toto",
        |      "tags":["test"],
        |      "slugs":["contactez-nous"],
        |      "linked_documents":[],
        |      "lang": "fr-fr",
        |       "data": {
        |         "pageweb": {
        |           "title": {
        |             "type": "StructuredText",
        |               "value": [{
        |                 "type": "heading1",
        |                 "text": "Contactez-nous",
        |                 "spans": [ ]
        |                        }]
        |                   },
        |           "body": {
        |                 "type": "SliceZone",
        |                 "value": [{
        |                           "type": "Slice",
        |                           "slice_type": "text",
        |                           "slice_label": null,
        |                           "repeat": [{ }],
        |                           "non-repeat": {
        |                                "text": {
        |                                  "type": "StructuredText",
        |                                  "value": [{
        |                                     "type": "paragraph",
        |                                     "text": "Contactez-nous pour toute demande de devis",
        |                                     "spans": [ ]
        |                                   }]
        |                                }
        |                            }
        |                         }]
        |                 }
        |             }
        |       }
        |   }
      """.stripMargin)
    val struct2 = json2.convertTo[Document].getSliceZone("pageweb.body")

    "not output a Seq and flatten in CompositeSlice asHTML" in {
      struct2 must beSome.like { case blocks: SliceZone =>
        blocks.asHtml(resolver).stripMargin.stripLineEnd mustEqual
          """<div data-slicetype="text" class="slice">
            |<div class="non-repeat">
            |<p>Contactez-nous pour toute demande de devis</p>
            |</div>
            |
            |</div>""".stripMargin
      }
    }
  }

  "StructuredText" should {
    "deserialize to simple text" in{
      val json = JsonParser(
        """
          | [
          |   {
          |   "type":"paragraph",
          |   "direction":"rtl",
          |   "text":"coucou",
          |   "spans":[]
          |   }
          |]
        """.stripMargin)
      json.convertTo[StructuredText].asHtml(DocumentLinkResolver(_ => "")) mustEqual "<p dir=\"rtl\">coucou</p>"
    }
  }

  "Slice Separator" should {
    val json = JsonParser(
      """
        | {
        |   "id":"VR1QBCcAACkArL-K",
        |   "uid":"website-starter-sample-page",
        |   "type":"page",
        |   "href":"https://blogtemplate.prismic.io/api/documents/search?ref=VakunBwAADkAnujS&q=%5B%5B%3Ad+%3D+at%28document.id%2C+%22VR1QBCcAACkArL-K%22%29+%5D%5D",
        |   "tags":[],
        |   "slugs":["website-starter-sample-page","sample-website-starter-page","a-sample-website-starter-page"],
        |   "linked_documents":[],
        |   "lang": "fr-fr",
        |   "data":{
        |     "page":{
        |       "body":{
        |         "type":"SliceZone",
        |         "value":[{
        |           "type":"Slice",
        |           "slice_type":"slides",
        |           "slice_label":null,
        |           "value":{
        |             "type":"Group","value":[{"illustration":{"type":"Image","value":{"main":{"url":"https://prismic-io.s3.amazonaws.com/blogtemplate/05f6ddc9efed8bf3b03e3847124b2015367d2ae3_slide.png","alt":"","copyright":"","dimensions":{"width":3760,"height":1918}},"views":{}}},"title":{"type":"StructuredText","value":[{"type":"heading1","text":"Website Starter Sample Page","spans":[]}]},"summary":{"type":"StructuredText","value":[{"type":"paragraph","text":"Lorem ipsum dolor sit amet, consectetur adipiscing elit.","spans":[]}]},"read-more-label":{"type":"StructuredText","value":[{"type":"paragraph","text":"button","spans":[]}]}},{"illustration":{"type":"Image","value":{"main":{"url":"https://prismic-io.s3.amazonaws.com/blogtemplate/05f6ddc9efed8bf3b03e3847124b2015367d2ae3_slide.png","alt":"","copyright":"","dimensions":{"width":3760,"height":1918}},"views":{}}},"title":{"type":"StructuredText","value":[{"type":"heading1","text":"Slide 2","spans":[]}]},"summary":{"type":"StructuredText","value":[{"type":"paragraph","text":"Sed ut perspiciatis, unde omnis iste natus error sit voluptatem.","spans":[]}]},"read-more-label":{"type":"StructuredText","value":[{"type":"paragraph","text":"button","spans":[]}]}},{"illustration":{"type":"Image","value":{"main":{"url":"https://prismic-io.s3.amazonaws.com/blogtemplate/05f6ddc9efed8bf3b03e3847124b2015367d2ae3_slide.png","alt":"","copyright":"","dimensions":{"width":3760,"height":1918}},"views":{}}},"title":{"type":"StructuredText","value":[{"type":"heading1","text":"Slide 3","spans":[]}]},"summary":{"type":"StructuredText","value":[{"type":"paragraph","text":"Itaque earum rerum hic tenetur a sapiente delectus.","spans":[]}]},"read-more-label":{"type":"StructuredText","value":[{"type":"paragraph","text":"button","spans":[]}]}}]
        |           }
        |         },{
        |           "type":"Slice",
        |           "slice_type":"separator",
        |           "slice_label":null,
        |           "value":{
        |             "type":"Group","value":[{"sep":{"type":"Separator"}}]
        |           }
        |         }]
        |       }
        |     }
        |   }
      }""".stripMargin
    )
    val struct = json.convertTo[Document].getSliceZone("page.body")
    "serialize to html" in {
      struct must beSome.like { case blocks: SliceZone =>
        println("")
        println(blocks.asHtml(resolver))
        println("")
        blocks.asHtml(resolver) mustEqual
          """<div data-slicetype="slides" class="slice"><section data-field="illustration"><img alt="" src="https://prismic-io.s3.amazonaws.com/blogtemplate/05f6ddc9efed8bf3b03e3847124b2015367d2ae3_slide.png" width="3760" height="1918" /></section>
          |<section data-field="title"><h1>Website Starter Sample Page</h1></section>
          |<section data-field="summary"><p>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p></section>
          |<section data-field="read-more-label"><p>button</p></section>
          |<section data-field="illustration"><img alt="" src="https://prismic-io.s3.amazonaws.com/blogtemplate/05f6ddc9efed8bf3b03e3847124b2015367d2ae3_slide.png" width="3760" height="1918" /></section>
          |<section data-field="title"><h1>Slide 2</h1></section>
          |<section data-field="summary"><p>Sed ut perspiciatis, unde omnis iste natus error sit voluptatem.</p></section>
          |<section data-field="read-more-label"><p>button</p></section>
          |<section data-field="illustration"><img alt="" src="https://prismic-io.s3.amazonaws.com/blogtemplate/05f6ddc9efed8bf3b03e3847124b2015367d2ae3_slide.png" width="3760" height="1918" /></section>
          |<section data-field="title"><h1>Slide 3</h1></section>
          |<section data-field="summary"><p>Itaque earum rerum hic tenetur a sapiente delectus.</p></section>
          |<section data-field="read-more-label"><p>button</p></section></div>
          |<div data-slicetype="separator" class="slice"><section data-field="sep"><hr/></section></div>""".stripMargin
        }
      }
  }

  "Nested spans" should {
    val text = "abcdefghijklmnopqrstuvwxyz"
    "correctly serialize with the same starting point" in {
      Block.asHtml(Block.Paragraph(
        text,
        Seq(
          Span.Em(2, 6),
          Span.Strong(2, 4)
        ), None, None), resolver) mustEqual "<p>ab<em><strong>cd</strong>ef</em>ghijklmnopqrstuvwxyz</p>"
    }
    "correctly serialize with the same starting point (2)" in {
      Block.asHtml(Block.Paragraph(
        text,
        Seq(
          Span.Em(2, 4),
          Span.Strong(2, 6)
        ), None, None), resolver) mustEqual "<p>ab<strong><em>cd</em>ef</strong>ghijklmnopqrstuvwxyz</p>"
    }
    "correctly serialize with a span taking the whole block" in {
      Block.asHtml(Block.Paragraph(
        "Javascript",
        Seq(
          Span.Em(0, 10)
        ), None, None), resolver) mustEqual "<p><em>Javascript</em></p>"
    }
    "correctly serialize with 2 spans taking the whole block" in {
      Block.asHtml(Block.Paragraph(
        "Javascript",
        Seq(
          Span.Em(0, 10),
          Span.Strong(0, 10)
        ), None, None), resolver) mustEqual "<p><em><strong>Javascript</strong></em></p>"
    }
  }

  "Block spans" should {
    "serialize labels within <li> tags" in {
      val json = JsonParser( """{"page":1,"results_per_page":20,"results_size":1,"total_results_size":1,"total_pages":1,"next_page":null,"prev_page":null,"results":[{"id":"VBgfNTYAANcgz2bT","type":"doc","href":"https://wroom.prismic.io/api/documents/search?ref=VDaF_jIAADMA7e5N&q=%5B%5B%3Ad+%3D+at%28document.id%2C+%22VBgfNTYAANcgz2bT%22%29+%5D%5D","tags":["doc-developers"],"slugs":["querying-a-repository"],"linked_documents":[{"id":"UkTD57O53-4AY1EF","tags":["api"],"type":"doc","slug":"api-documentation","lang":"en-en"},{"id":"UmlghEnM00YhgHUB","tags":["api"],"type":"doc","slug":"orderings","lang":"en-en"}],"lang": "fr-fr", "data":{"doc":{"title":{"type":"StructuredText","value":[{"type":"heading1","text":"Querying a Repository","spans":[]}]},"content":{"type":"StructuredText","value":[{"type":"paragraph","direction":"rtl","text":"To query your API, you will need to specify a form and a reference in addition to your query.","spans":[{"start":46,"end":50,"type":"strong"},{"start":57,"end":67,"type":"strong"},{"start":78,"end":92,"type":"strong"}]},{"type":"list-item","text":"The operator: this is the function you call to build the predicate, for example Predicate.at.","spans":[{"start":4,"end":12,"type":"em"},{"start":80,"end":93,"type":"label","data":{"label":"codespan"}}]},{"type":"list-item","text":"The fragment: the first argument you pass, for example \"document.id\".","spans":[{"start":4,"end":12,"type":"em"},{"start":55,"end":68,"type":"label","data":{"label":"codespan"}}]},{"type":"list-item","text":"The values: the other arguments you pass, usually one but it can be more for some predicates. For example \"product\".","spans":[{"start":4,"end":10,"type":"em"},{"start":106,"end":115,"type":"label","data":{"label":"codespan"}}]}]}}}}],"version":"e5752a1","license":"All Rights Reserved"}""")
      val response = json.convertTo[Response]
      val text = response.results.head.getStructuredText("doc.content")
      text.map(_.asHtml(resolver)) must beSome.like {
        case html => html mustEqual
          """<p dir="rtl">To query your API, you will need to specify a <strong>form</strong> and a <strong>reference </strong>in addition<strong> to your query</strong>.</p>
            |
            |<ul>
            |
            |<li>The <em>operator</em>: this is the function you call to build the predicate, for example <span class="codespan">Predicate.at.</span></li>
            |
            |<li>The <em>fragment</em>: the first argument you pass, for example <span class="codespan">"document.id"</span>.</li>
            |
            |<li>The <em>values</em>: the other arguments you pass, usually one but it can be more for some predicates. For example <span class="codespan">"product"</span>.</li>
            |
            |</ul>""".stripMargin
      }
    }
    "serialize starting at the beginning" in {
      val json = JsonParser(
        """
          |[{
          |  "type": "paragraph",
          |  "text": "This section describes the REST API to access Prismic.io. It is useful to get an in-depth knowledge on how Prismic.io works, but most of the time you will be using a development kit; for that reason it is recommended to get familiar with the developer's manual before proceeding on this section.",
          |  "spans": [{
          |    "start": 0,
          |    "end": 242,
          |    "type": "em"
          |  }, {
          |    "start": 242,
          |    "end": 260,
          |    "type": "hyperlink",
          |    "data": {
          |      "type": "Link.document",
          |      "value": {
          |        "document": {
          |          "id": "VBgeDDYAADMAz2Rw",
          |          "type": "documentation-categoy",
          |          "tags": ["doc-developers"],
          |          "slug": "developers-manual",
          |          "lang": "en-en"
          |        },
          |        "isBroken": false
          |      }
          |    }
          |  }, {
          |    "start": 242,
          |    "end": 260,
          |    "type": "em"
          |  }, {
          |    "start": 260,
          |    "end": 295,
          |    "type": "em"
          |  }]
          |}]
        """.stripMargin)
      json.convertTo[StructuredText].asHtml(resolver) mustEqual "<p><em>This section describes the REST API to access Prismic.io. It is useful to get an in-depth knowledge on how Prismic.io works, but most of the time you will be using a development kit; for that reason it is recommended to get familiar with the </em><a href=\"http://localhost/documentation-categoy/VBgeDDYAADMAz2Rw\"><em>developer's manual</em></a><em> before proceeding on this section.</em></p>"
    }
  }

	"Image as link" should {
		val json = JsonParser(
			"""
				|{
				|    "copyright": null,
				|    "url": "http://cdn.sentione.com/season-logo/logo.png",
				|    "linkTo": {
				|      "type": "Link.web",
				|      "value": {
				|        "url": "http://sentione.com"
				|      }
				|    },
				|    "alt": null,
				|    "dimensions": {
				|      "width": 180,
				|      "height": 82
				|    },
				|    "type": "image"
				|}
			""".stripMargin)
		"deserialize to image block with linkTo value set" in {
			json.convertTo[Block].asInstanceOf[Block.Image].linkTo.isDefined mustEqual true
		}
		"render as image as link" in {
			StructuredText(Seq(json.convertTo[Block])).asHtml(DocumentLinkResolver(_ => "")) mustEqual
				"<p class=\"block-img\"><a href=\"http://sentione.com\"><img alt=\"\" src=\"http://cdn.sentione.com/season-logo/logo.png\" width=\"180\" height=\"82\" /></a></p>"
		}
	}

  "WebLink" should {

    "deserialize to weblink without target" in {
      val json = JsonParser(
			"""
				|{
        |  "url": "https://google.fr"
        |}
			""".stripMargin)

			json.convertTo[WebLink].asHtml() mustEqual """<a href="https://google.fr">https://google.fr</a>"""
		}

    "deserialize to weblink with target blank" in {
      val json = JsonParser(
			"""
				|{
        |  "url": "https://google.fr",
        |  "target": "_blank"
        |}
			""".stripMargin)

			json.convertTo[WebLink].asHtml() mustEqual """<a href="https://google.fr" target="_blank" rel="noopener">https://google.fr</a>"""
		}
  }

  "FileLink" should {
    "deserialize to fileLink without target" in {
      val json = JsonParser(
			"""
				|{
        |  "file": {
        |    "url": "https://mondoc.com",
        |    "kind": "media",
        |    "size": "10",
        |    "name": "mondoc"
        |  }
        |}
			""".stripMargin)

			json.convertTo[FileLink].asHtml() mustEqual """<a href="https://mondoc.com">mondoc</a>"""
		}

    "deserialize to fileLink with target" in {
      val json = JsonParser(
			"""
				|{
        |  "file": {
        |    "url": "https://mondoc.com",
        |    "kind": "media",
        |    "size": "10",
        |    "name": "mondoc",
        |    "target": "_blank"
        |  }
        |}
			""".stripMargin)

			json.convertTo[FileLink].asHtml() mustEqual """<a href="https://mondoc.com" target="_blank" rel="noopener">mondoc</a>"""
		}
  }

  "ImageLink" should {
    "deserialize to ImageLink without target" in {
      val json = JsonParser(
			"""
				|{
        |  "image": {
        |    "url": "https://monmedia.com",
        |    "kind": "media",
        |    "size": "10",
        |    "name": "monmedia"
        |  }
        |}
			""".stripMargin)

			json.convertTo[ImageLink].asHtml() mustEqual """<img src="https://monmedia.com" alt="monmedia"/>"""
		}

    "deserialize to ImageLink with target" in {
      val json = JsonParser(
			"""
				|{
        |  "image": {
        |    "url": "https://monmedia.com",
        |    "kind": "media",
        |    "size": "10",
        |    "name": "monmedia",
        |    "target": "_blank"
        |  }
        |}
			""".stripMargin)

			json.convertTo[ImageLink].asHtml() mustEqual """<a href="https://monmedia.com" target="_blank" rel="noopener"><img src="https://monmedia.com" alt="monmedia"/></a>"""
		}
  }
}
