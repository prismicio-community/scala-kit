package io.prismic

import spray.json._

import scala.language.implicitConversions
import scala.util.control.Exception._

class PrismicJson(json: JsValue) {

  def \(field: String): JsValue = json match {
    case o: JsObject => o.getFields(field) match {
      case Seq(value) => value
      case _ => JsNull
    }
    case _ => JsNull
  }

  def validate[T: JsonReader]: Either[Throwable, T] = catching(classOf[DeserializationException]) either {
    json.convertTo[T]
  }

  def toOpt[T: JsonReader]: Option[T] = catching(classOf[DeserializationException]) opt {
    json.convertTo[T]
  }

}

object PrismicJson {

  implicit def toPrismicJson(json: JsValue): PrismicJson = new PrismicJson(json)

}
