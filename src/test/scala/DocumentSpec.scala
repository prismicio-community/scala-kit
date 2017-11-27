package io.prismic

import org.joda.time.{ DateTime, DateTimeZone }
import org.specs2.mutable._
import spray.json._
import io.prismic.PrismicJsonProtocol._

class DocumentSpec extends Specification {

  def fixture(file: String): JsValue = {
    val content = scala.io.Source.fromFile(s"src/test/scala/fixtures/$file").mkString
    JsonParser(content)
  }

  lazy val document = fixture("document_store.json").convertTo[Document]

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
  }
}
