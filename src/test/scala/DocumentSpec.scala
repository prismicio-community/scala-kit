package io.prismic

import org.joda.time.{DateTime, DateTimeZone}
import org.specs2.mutable._
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
  }
}
