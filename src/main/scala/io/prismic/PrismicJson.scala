package io.prismic

import spray.json._

class PrismicJson(json: JsValue) {

  def \(field: String): JsValue = json match {
    case o: JsObject => o.getFields(field) match {
      case Seq(value) => value
    }
    case _ => JsNull
  }

}

object PrismicJson {

  implicit def toPrismicJson(json: JsValue) = new PrismicJson(json)

}
