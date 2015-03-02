package io.prismic

import scala.language.implicitConversions
import scala.util.control.Exception._
import spray.json._

class PrismicJson(json: JsValue) {

  def \(field: String): JsValue = json match {
    case o: JsObject => o.getFields(field) match {
      case Seq(value) => value
    }
    case _ => JsNull
  }

  def validate[T](field: String): Either[Throwable, T] = catching(classOf[DeserializationException]) either {
    json.convertTo[T]
  }

  def toOpt[T]: Option[T] = catching(classOf[DeserializationException]) opt {
    json.convertTo[T]
  }

}

object PrismicJson {

  implicit def toPrismicJson(json: JsValue) = new PrismicJson(json)

}
