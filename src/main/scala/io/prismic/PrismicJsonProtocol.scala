package io.prismic

import org.joda.time._

import io.prismic.Fragment._
import spray.json._

import scala.util.Try


object PrismicJsonProtocol extends DefaultJsonProtocol {

  import PrismicJson._

  implicit object WeblinkReader extends RootJsonReader[WebLink] {
    override def read(json: JsValue): WebLink = json.asJsObject.getFields("url") match {
      case Seq(JsString(url)) => WebLink(url)
      case _ => throw new DeserializationException("Expected url field")
    }
  }

  implicit object MediaLinkReader extends RootJsonReader[MediaLink] {
    override def read(json: JsValue): MediaLink = (json \ "file").asJsObject.getFields("url", "kind", "size", "name") match {
      case Seq(JsString(url), JsString(kind), JsString(size), JsString(name)) => MediaLink(url, kind, size.toLong, name)
      case _ => throw new DeserializationException("Missing field")
    }
  }

  implicit object DateReader extends RootJsonReader[Date] {
    override def read(json: JsValue): Date =
      Try(Date(LocalDate.parse(json.asInstanceOf[String], format.DateTimeFormat.forPattern("yyyy-MM-dd")))).getOrElse {
        throw new DeserializationException("date parsing error")
    }
  }

  implicit object TextReader extends RootJsonReader[Text] {
    override def read(json: JsValue): Text = Text(json.asInstanceOf[String])
  }

  implicit object TimestampReader extends RootJsonReader[Timestamp] {
    override def read(json: JsValue): Timestamp = {
      val d = json.convertTo[String]
        val isoFormat = org.joda.time.format.DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ")
        Try(Timestamp(DateTime.parse(d, isoFormat).withZone(DateTimeZone.UTC)))
          .getOrElse(throw new DeserializationException(s"Invalid timestamp value $v"))
    }
  }

  implicit object NumberReader extends RootJsonReader[Number] {
    override def read(json: JsValue): Number = Number(json.asInstanceOf[Double])
  }

  implicit object ColorReader extends RootJsonReader[Color] {
    override def read(json: JsValue): Color = json match {
      case JsString(hex) =>
        if (Color.isValidColorValue(hex)) {
          Color(hex)
        } else {
          throw new DeserializationException(s"Invalid color value $hex")
        }
      case _ => throw new DeserializationException(s"Expected String")
    }
  }

  implicit object EmbedReader extends RootJsonReader[Embed] {
    override def read(json: JsValue): Embed = json.asJsObject.getFields("oembed") match {
      case Seq(oembed) => oembed.asJsObject.getFields("type", "provider_name", "embed_url", "width", "height", "html") match {
        case (JsString(typ), provider:JsValue, JsString(url), width:JsValue, height:JsValue, html) =>
          Embed(typ, provider.convertTo[Option[String]], url, width.convertTo[Option[Int]], height.convertTo[Option[Int]], html, oembed)
      }
    }
  }

  implicit object GeoPointReader extends RootJsonReader[GeoPoint] {
    override def read(json: JsValue): GeoPoint = json.asJsObject.getFields("latitude", "longitude") match {
      case Seq(JsNumber(latitude), JsNumber(longitude)) => GeoPoint(latitude.toDouble, longitude.toDouble)
      case _ => throw new DeserializationException("Expected longitude and latitude as numbers")
    }
  }

}


