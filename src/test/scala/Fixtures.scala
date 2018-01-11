package io.prismic

import PrismicJsonProtocol._
import spray.json._

object Fixtures {

  lazy val document: Document = Fixtures.load("document_store.json").convertTo[Document]

  def load(file: String): JsValue = {
    val content = scala.io.Source.fromFile(s"src/test/scala/fixtures/$file").mkString
    JsonParser(content)
  }
}
