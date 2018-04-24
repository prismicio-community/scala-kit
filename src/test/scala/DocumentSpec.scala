package io.prismic

import io.prismic.fragments._
import org.joda.time.{DateTime, DateTimeZone}
import org.specs2.mutable._
import spray.json._
import io.prismic.PrismicJsonProtocol._
import io.prismic.fragments.AlternateLanguage

class DocumentSpec extends Specification {

  lazy val document = Fixtures.document

  "Document" should {
    "have first publication" in {
      Some(DateTime.now()
        .withZone(DateTimeZone.UTC)
        .withYear(2017)
        .withMonthOfYear(1)
        .withDayOfMonth(13)
        .withHourOfDay(11)
        .withMinuteOfHour(45)
        .withSecondOfMinute(21)
        .withMillisOfSecond(0)) must_==(document.firstPublicationDate)
    }

    "should have last publication date" in {
      Some(DateTime.now()
        .withZone(DateTimeZone.UTC)
        .withYear(2017)
        .withMonthOfYear(2)
        .withDayOfMonth(21)
        .withHourOfDay(16)
        .withMinuteOfHour(5)
        .withSecondOfMinute(19)
        .withMillisOfSecond(0)) must_==(document.lastPublicationDate)
    }

    "should have lang" in {
      "fr-fr" must_== document.lang
    }

    "should have alternative languages" in {
      document.alternateLanguages must
        have size 1 and
        contain(AlternateLanguage("WmWkEyQAABvqGKrj", None, "store", "de-de"))
    }

    "should parse fetch links" in {
      println("json")

      val json = JsonParser(
        """
        | {
        |  "id": "WRmlvyMAANV2plTU",
        |  "uid": null,
        |  "type": "home",
        |  "href": "https://prismicio-docs.cdn.prismic.io/api/v1/documents/search?ref=Wt4EwCcAAB1hFCNZ&q=%5B%5B%3Ad+%3D+at%28document.id%2C+%22WRmlvyMAANV2plTU%22%29+%5D%5D",
        |  "tags": [],
        |  "first_publication_date": "2017-05-15T13:52:06+0000",
        |  "last_publication_date": "2018-03-13T14:11:05+0000",
        |  "slugs": [
        |    "prismic.io-documentation",
        |    "api-clients"
        |  ],
        |  "linked_documents": null,
        |  "lang": "en-us",
        |  "alternate_languages": [],
        |  "data": {
        |   "home": {
        |     "link": {
        |       "type": "Link.document",
        |       "value": {
        |         "document": {
        |           "id": "WTquXikAACkA4Hux",
        |           "type": "page-javascript",
        |           "tags": [],
        |           "lang": "en-us",
        |           "slug": "page---javascript---integrating-with-an-existing-project",
        |           "uid": "integrating-with-an-existing-javascript-project",
        |           "data": {
        |             "page-javascript": {
        |               "category": {
        |                 "type": "Link.document",
        |                 "value": {
        |                   "document": {
        |                     "id": "WTl0SikAACYA2xXL",
        |                     "lang": "en-us",
        |                     "type": "category-javascript",
        |                     "tags": [],
        |                     "slug": "category---javascript---getting-started",
        |                     "uid": "getting-started"
        |                   }
        |                 },
        |                 "isBroken": false
        |               }
        |             }
        |           }
        |         },
        |         "isBroken": false
        |       }
        |     }
        |   }
        |  }
        |}
      """.stripMargin)

      val doc = json.convertTo[Document]

      val link = doc.fragments.get("home.link").get
      val computed = link.asInstanceOf[DocumentLink].fragments.size


      computed must_== 1
    }
  }
}
