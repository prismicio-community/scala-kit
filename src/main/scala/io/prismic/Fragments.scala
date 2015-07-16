package io.prismic

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.{ postfixOps, implicitConversions }
import scala.util._

import io.prismic.fragments._

trait HtmlSerializer {
  def apply(elt: Fragment.StructuredText.Element, content: String): Option[String]
}

object HtmlSerializer {
  import Fragment.StructuredText._

  def apply(f: PartialFunction[(Element, String), String]) = new HtmlSerializer {
    override def apply(elt: Element, content: String) =
      if (f.isDefinedAt((elt, content))) {
        Some(f.apply(elt, content))
      } else {
        None
      }
  }

  def empty = new HtmlSerializer {
    override def apply(elts: Element, content: String): Option[String] = None
  }

}

trait Fragment

object Fragment {

  @deprecated("Use io.prismic.fragments.Link", "1.3.3")
  type Link = io.prismic.fragments.Link

  @deprecated("Use io.prismic.fragments.WebLink", "1.3.3")
  type WebLink = io.prismic.fragments.WebLink
  @deprecated("Use io.prismic.fragments.WebLink", "1.3.3")
  val WebLink = io.prismic.fragments.WebLink

  @deprecated("Use io.prismic.fragments.FileLink", "1.3.3")
  type MediaLink = io.prismic.fragments.FileLink
  @deprecated("Use io.prismic.fragments.FileLink", "1.3.3")
  val MediaLink = io.prismic.fragments.FileLink

  @deprecated("Use io.prismic.fragments.DocumentLink", "1.3.3")
  type DocumentLink = io.prismic.fragments.DocumentLink
  @deprecated("Use io.prismic.fragments.DocumentLink", "1.3.3")
  val DocumentLink = io.prismic.fragments.DocumentLink

  // ------------------

  @deprecated("Use io.prismic.fragments.Text", "1.3.3")
  type Text = io.prismic.fragments.Text
  @deprecated("Use io.prismic.fragments.Text", "1.3.3")
  val Text = io.prismic.fragments.Text

  // ------------------

  @deprecated("Use io.prismic.fragments.Date", "1.3.3")
  type Date = io.prismic.fragments.Date
  @deprecated("Use io.prismic.fragments.Date", "1.3.3")
  val Date = io.prismic.fragments.Date

  @deprecated("Use io.prismic.fragments.Timestamp", "1.3.3")
  type Timestamp = io.prismic.fragments.Timestamp
  @deprecated("Use io.prismic.fragments.Timestamp", "1.3.3")
  val Timestamp = io.prismic.fragments.Timestamp

  // ------------------

  @deprecated("Use io.prismic.fragments.Number", "1.3.3")
  type Number = io.prismic.fragments.Number
  @deprecated("Use io.prismic.fragments.Number", "1.3.3")
  val Number = io.prismic.fragments.Number

  // ------------------

  @deprecated("Use io.prismic.fragments.Color", "1.3.3")
  type Color = io.prismic.fragments.Color
  @deprecated("Use io.prismic.fragments.Color", "1.3.3")
  val Color = io.prismic.fragments.Color

  // ------------------

  @deprecated("Use io.prismic.fragments.Embed", "1.3.3")
  type Embed = io.prismic.fragments.Embed
  @deprecated("Use io.prismic.fragments.Embed", "1.3.3")
  val Embed = io.prismic.fragments.Embed

  // ------------------

  @deprecated("Use io.prismic.fragments.GeoPoint", "1.3.3")
  type GeoPoint = io.prismic.fragments.GeoPoint
  @deprecated("Use io.prismic.fragments.GeoPoint", "1.3.3")
  val GeoPoint = io.prismic.fragments.GeoPoint

  // ------------------

  @deprecated("Use io.prismic.fragments.Image", "1.3.3")
  type Image = io.prismic.fragments.Image
  @deprecated("Use io.prismic.fragments.Image", "1.3.3")
  val Image = io.prismic.fragments.Image

  @deprecated("Use io.prismic.fragments.Group", "1.3.3")
  type Group = io.prismic.fragments.Group
  @deprecated("Use io.prismic.fragments.Group", "1.3.3")
  val Group = io.prismic.fragments.Group

  @deprecated("Use io.prismic.fragments.StructuredText", "1.3.3")
  type StructuredText = io.prismic.fragments.StructuredText
  @deprecated("Use io.prismic.fragments.StructuredText", "1.3.3")
  val StructuredText = io.prismic.fragments.StructuredText


  def getHtml(fragment: Fragment, linkResolver: DocumentLinkResolver): String = fragment match {
    case a: StructuredText => a.asHtml(linkResolver)
    case a: Number         => a.asHtml
    case a: Color          => a.asHtml
    case a: Text           => a.asHtml
    case a: Date           => a.asHtml
    case a: Timestamp      => a.asHtml
    case a: Embed          => a.asHtml()
    case a: Image          => a.asHtml
    case a: WebLink        => a.asHtml
    case a: FileLink       => a.asHtml
    case a: GeoPoint       => a.asHtml
    case a: DocumentLink   => a.asHtml(linkResolver)
    case a: Group          => a asHtml linkResolver
    case a: SliceZone      => a asHtml(linkResolver)
  }

}
