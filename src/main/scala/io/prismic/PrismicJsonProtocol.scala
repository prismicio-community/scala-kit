package io.prismic

import io.prismic.Fragment._
import spray.json._


object PrismicJsonProtocol extends DefaultJsonProtocol {

  implicit object WeblinkReader extends RootJsonReader[WebLink] {
    override def read(json: JsValue): WebLink = json.asJsObject.getFields("url") match {
      case Seq(JsString(url)) => WebLink(url)
      case _ => throw new DeserializationException("Expected url field")
    }
  }

  implicit object MediaLinkReader extends RootJsonReader[MediaLink] {
    override def read(json: JsValue): MediaLink = json.asJsObject.getFields("file") match {
      case Seq(file) => file.asJsObject.getFields("url", "kind", "size", "name") match {
        case Seq(JsString(url), JsString(kind), JsString(size), JsString(name)) => MediaLink(url, kind, size.toLong, name)
        case _ => throw new DeserializationException("Missing field")
      }
      case _ => throw new DeserializationException("missing 'file' attribute")
    }
  }

}
