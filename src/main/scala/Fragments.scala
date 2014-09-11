package io.prismic

import org.joda.time._

import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.{ postfixOps, implicitConversions }
import scala.util._

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

sealed trait Fragment

object Fragment {

  sealed trait Link extends Fragment

  case class WebLink(url: String, contentType: Option[String] = None) extends Link {
    def asHtml(): String = s"""<a href="$url">$url</a>"""
  }

  object WebLink {

    implicit val reader: Reads[WebLink] = {
      (__ \ "url").read[String].map {
        case url => WebLink(url)
      }
    }

  }

  case class MediaLink(url: String, kind: String, size: Long, filename: String) extends Link {
    def asHtml: String = s"""<a href="$url">$filename</a>"""
  }

  object MediaLink {

    implicit val reader: Reads[MediaLink] = {
      (
        (__ \ "file" \ "url").read[String] and
        (__ \ "file" \ "kind").read[String] and
        (__ \ "file" \ "size").read[String].map(_.toLong) and
        (__ \ "file" \ "name").read[String]
      )(MediaLink.apply _)
    }
  }

  case class DocumentLink(id: String, typ: String, tags: Seq[String], slug: String, isBroken: Boolean) extends Link {
    def asHtml(linkResolver: DocumentLinkResolver): String = s"""<a href="${linkResolver(this)}">$slug</a>"""
  }

  object DocumentLink {

    implicit val reader: Reads[DocumentLink] = {
      (
        (__ \ "document").read(
          (__ \ "id").read[String] and
            (__ \ "type").read[String] and
            (__ \ "tags").readNullable[Seq[String]].map(_.getOrElse(Nil)) and
            (__ \ "slug").read[String] tupled
        ) and
          (__ \ "isBroken").readNullable[Boolean].map(_.getOrElse(false))
      ).tupled.map(link => DocumentLink(link._1._1, link._1._2, link._1._3, link._1._4, link._2))
    }

  }

  object Link {
    implicit val reader = Reads[Link] { jsvalue =>
      (jsvalue \ "type").validate[String] flatMap {
        case "Link.web" => Fragment.WebLink.reader.reads(jsvalue)
        case "Link.document" => Fragment.DocumentLink.reader.reads(jsvalue)
        case "Link.file" => Fragment.MediaLink.reader.reads(jsvalue)
      }
    }
  }

  // ------------------

  case class Text(value: String) extends Fragment {
    def asHtml: String = s"""<span class="text">$value</span>"""
  }

  object Text {
    implicit val reader: Reads[Text] = {
      Reads(v => v.asOpt[String].map(d => JsSuccess(Text(d))).getOrElse(JsError(s"Invalid text value $v")))
    }
  }

  // ------------------

  case class Date(value: DateTime) extends Fragment {
    def asText(pattern: String) = value.toString(pattern)
    def asHtml: String = s"""<time>$value</time>"""
  }

  object Date {
    implicit val reader: Reads[Date] = {
      Reads(v => v.asOpt[String].flatMap(d => Try(JsSuccess(Date(DateTime.parse(d, format.DateTimeFormat.forPattern("yyyy-MM-dd"))))).toOption).getOrElse(JsError(s"Invalid date value $v")))
    }
  }

  // ------------------

  case class Number(value: Double) extends Fragment {
    def asInt = value.toInt
    def asText(pattern: String) = new java.text.DecimalFormat(pattern).format(value)
    def asHtml: String = s"""<span class="number">$value</span>"""
  }

  object Number {
    implicit val reader: Reads[Number] = {
      Reads(v => v.asOpt[Double].map(d => JsSuccess(Number(d))).getOrElse(JsError(s"Invalid number value $v")))
    }
  }

  // ------------------

  case class Color(hex: String) extends Fragment {
    def asRGB = Color.asRGB(hex)
    def asHtml: String = s"""<span class="color">$hex</span>"""
  }

  object Color {

    private val HexColor = """#([a-fA-F0-9]{2})([a-fA-F0-9]{2})([a-fA-F0-9]{2})""".r

    def isValidColorValue(hex: String): Boolean = hex match {
      case HexColor(r, g, b) => true
      case _                 => false
    }

    def asRGB(hex: String): (Int, Int, Int) = hex match {
      case HexColor(r, g, b) => (Integer.parseInt(r, 16), Integer.parseInt(g, 16), Integer.parseInt(b, 16))
      case _                 => (0, 0, 0)
    }

    implicit val reader: Reads[Color] = {
      Reads(v => v.asOpt[String].filter(isValidColorValue).map(hex => JsSuccess(Color(hex))).getOrElse(JsError(s"Invalid color value $v")))
    }

  }

  // ------------------

  case class Embed(typ: String, provider: String, url: String, width: Option[Int], height: Option[Int], html: Option[String], oembedJson: JsValue) extends Fragment {
    def asHtml: String = {
      html.map(html => s"""<div data-oembed="$url" data-oembed-type="${typ.toLowerCase}" data-oembed-provider="${provider.toLowerCase}">$html</div>""").getOrElse("")
    }
  }

  object Embed {

    implicit val reader: Reads[Embed] = {
      (__ \ "oembed").read(
        (
          (__ \ "type").read[String] and
          (__ \ "provider_name").read[String] and
          (__ \ "embed_url").read[String] and
          (__ \ "width").readNullable[Int] and
          (__ \ "height").readNullable[Int] and
          (__ \ "html").readNullable[String] and
          __.read[JsObject]
        )(Embed.apply _)
      )
    }

  }

  // ------------------

  case class GeoPoint(latitude: Double, longitude: Double) extends Fragment {
    def asHtml: String = s"""<div class="geopoint"><span class="latitude">${latitude}</span><span class="longitude">${longitude}</span></div>"""
  }

  object GeoPoint {

    implicit val reader: Reads[GeoPoint] = {
      (
        (__ \ "latitude").read[Double] and
        (__ \ "longitude").read[Double]
      )(GeoPoint.apply _)
    }

  }

  // ------------------

  case class Image(main: Image.View, views: Map[String, Image.View] = Map.empty) extends Fragment {

    def getView(key: String): Option[Image.View] = key.toLowerCase match {
      case "main" => Some(main)
      case _      => views.get(key)
    }

    def asHtml: String = main.asHtml

  }

  object Image {

    case class View(url: String, width: Int, height: Int, alt: Option[String]) {
      def ratio = width / height
      def asHtml: String = s"""<img alt="${alt.getOrElse("")}" src="${url}" width="${width}" height="${height}" />"""
    }

    implicit val viewReader: Reads[View] =
      (
        (__ \ 'url).read[String] and
        (__ \ 'dimensions).read(
          (__ \ 'width).read[Int] and
            (__ \ 'height).read[Int] tupled
        ) and
        (__ \ 'alt).read[Option[String]]
      ).tupled.map {
            case (url, (width, height), alt) => View(url, width, height, alt)
          }

    implicit val reader: Reads[Image] =
      (
        (__ \ 'main).read[View] and
        (__ \ 'views).read[Map[String, View]]
      ).tupled.map {
          case (main, views) => Image(main, views)
        }

  }

  case class Group(docs: Seq[Group.Doc]) extends Fragment {

    def asHtml(linkResolver: DocumentLinkResolver): String =
      docs map (_ asHtml linkResolver) mkString "\n"
  }

  object Group {

    case class Doc(fragments: Map[String, Fragment]) extends WithFragments

    private implicit val fragmentRead: Reads[Fragment] = Reads { value =>
      value.asOpt[JsObject] flatMap Document.parse match {
        case Some(f) => JsSuccess(f)
        case None    => JsError(Nil)
      }
    }

    implicit val reader: Reads[Group] =
      Reads.seq(__.read[Map[String, Fragment]]).map { docs =>
        Group(docs.map(Doc))
      }
  }

  case class StructuredText(blocks: Seq[StructuredText.Block]) extends Fragment {

    def getTitle: Option[StructuredText.Block.Heading] = blocks.collectFirst {
      case h: StructuredText.Block.Heading => h
    }

    def getFirstParagraph: Option[StructuredText.Block.Paragraph] = blocks.collectFirst {
      case p: StructuredText.Block.Paragraph => p
    }

    def getAllParagraphs: Seq[StructuredText.Block.Paragraph] = blocks.collect {
      case p: StructuredText.Block.Paragraph => p
    }

    def getFirstImage: Option[StructuredText.Block.Image] = blocks.collectFirst {
      case i: StructuredText.Block.Image => i
    }

    def asHtml(linkResolver: DocumentLinkResolver, htmlSerializer: HtmlSerializer = HtmlSerializer.empty): String = {
      StructuredText.asHtml(blocks, linkResolver, htmlSerializer)
    }

  }

  // ------------------

  object StructuredText {

    def asHtml(blocks: Seq[Block], linkResolver: DocumentLinkResolver, htmlSerializer: HtmlSerializer): String = {
      case class Group(htmlTag: Option[String], blocks: Seq[Block])

      val grouped: List[Group] = blocks.foldLeft(List.empty[Group]) {
        case ((group@Group(Some("ul"), _)) :: rest, block@StructuredText.Block.ListItem(text, spans, false, label)) => group.copy(blocks = group.blocks :+ block) +: rest
        case ((group@Group(Some("ol"), _)) :: rest, block@StructuredText.Block.ListItem(text, spans, true, label)) => group.copy(blocks = group.blocks :+ block) +: rest
        case (groups, block@StructuredText.Block.ListItem(text, spans, false, label)) => Group(Some("ul"), Seq(block)) +: groups
        case (groups, block@StructuredText.Block.ListItem(text, spans, true, label)) => Group(Some("ol"), Seq(block)) +: groups
        case (groups, block) => Group(None, Seq(block)) +: groups
      }.reverse

      grouped.flatMap {
        case Group(Some(tag), bcks) => s"<$tag>" +: bcks.map(block => Block.asHtml(block, linkResolver, htmlSerializer)) :+ s"</$tag>"
        case Group(None, bcks)      => bcks.map(block => Block.asHtml(block, linkResolver, htmlSerializer))
      }.mkString("\n\n")
    }

    @deprecated("Use Block.asHtml", "1.0.14")
    def asHtml(block: Block, linkResolver: DocumentLinkResolver): String = {
      Block.asHtml(block, linkResolver)
    }

    private def asHtml(text: String, spans: Seq[Span], linkResolver: DocumentLinkResolver, serializer: HtmlSerializer): String = {

      def escape(character: String): String = {
        character.replace("<", "&lt;").replace("\n", "<br>")
      }

      def serialize(element: Element, content: String): String = {
        serializer(element, content).getOrElse {
          val cls = element.label match {
            case Some(label) => s""" class="$label""""
            case None => ""
          }
          element match {
            case b: Block => Block.asHtml(b, linkResolver)
            case _: Span.Em => s"<em$cls>$content</em>"
            case _: Span.Strong => s"<strong$cls>$content</strong>"
            case Span.Hyperlink(_, _, link: DocumentLink, _) => s"""<a$cls href="${linkResolver(link)}">$content</a>"""
            case Span.Hyperlink(_, _, link: MediaLink, _) => s"""<a$cls href="${link.url}">$content</a>"""
            case Span.Hyperlink(_, _, link: WebLink, _) => s"""<a$cls href="${link.url}">$content</a>"""
            case _ => s"<span$cls>$content</span>"
          }
        }
      }

      case class OpenSpan(span: Span, content: String)

      @scala.annotation.tailrec
      def step(in: Seq[(Char, Int)], spans: Seq[Span], stack: Seq[OpenSpan] = Nil, html: String = ""): String = {
        in match {
          case ((_, pos) :: tail) if stack.headOption.map(_.span.end) == Some(pos) => {
            // Need to close a tag
            val tagHtml = serialize(stack.head.span, stack.head.content)
            stack.drop(1) match {
              case Nil => step(in, spans, Nil, html + tagHtml)
              case h :: t => step(in, spans, h.copy(content = h.content + tagHtml) :: t, html)
            }
          }
          case ((_, pos) :: tail) if spans.headOption.map(_.start) == Some(pos) => {
            // Need to open a tag
            step(in, spans.drop(1), OpenSpan(spans.head, "") +: stack, html)
          }
          case (current, pos) :: tail => {
            stack match {
              case Nil =>
                // Top level
                step(tail, spans, stack, html + escape(current.toString))
              case head :: t =>
                // There is an open span, insert inside
                step(tail, spans, head.copy(content = head.content + escape(current.toString)) :: t, html)
            }
          }
          case Nil => html
        }
      }
      step(text.toList.zipWithIndex, spans.sortBy(_.start))
    }

    sealed trait Element {
      def label: Option[String]
    }

    sealed trait Span extends Element {
      def start: Int
      def end: Int
    }

    object Span {

      case class Em(start: Int, end: Int, label: Option[String]) extends Span
      case class Strong(start: Int, end: Int, label: Option[String]) extends Span
      case class Hyperlink(start: Int, end: Int, link: Link, label: Option[String]) extends Span

      implicit val reader: Reads[Span] =
        (
          (__ \ 'type).read[String] and
          (__ \ 'start).read[Int] and
          (__ \ 'end).read[Int] and
          (__ \ 'data).readNullable[JsObject].map(_.getOrElse(Json.obj())) and
          (__ \ 'label).readNullable[String]
        ).tupled.flatMap {
            case (typ, start, end, data, label) => typ match {
              case "strong" => Reads.pure(Strong(start, end, label))
              case "em" => Reads.pure(Em(start, end, label))
              case "hyperlink" if (data \ "type").asOpt[String].exists(_ == "Link.web") => (__ \ "data" \ "value").read(WebLink.reader).map(link => Hyperlink(start, end, link, label))
              case "hyperlink" if (data \ "type").asOpt[String].exists(_ == "Link.document") => (__ \ "data" \ "value").read(DocumentLink.reader).map(link => Hyperlink(start, end, link, label))
              case "hyperlink" if (data \ "type").asOpt[String].exists(_ == "Link.file") => (__ \ "data" \ "value").read(MediaLink.reader).map(link => Hyperlink(start, end, link, label))
              case t => Reads(json => JsError(s"Unsupported span type $t"))
            }
          }
    }

    sealed trait Block extends Element

    object Block {

      sealed trait Text extends Block {
        def text: String
        def spans: Seq[Span]
        override def label: Option[String]
      }

      def asHtml(block: Block, linkResolver: DocumentLinkResolver, htmlSerializer: HtmlSerializer = HtmlSerializer.empty): String = {
        val cls = block.label match {
          case Some(label) => s""" class="$label""""
          case None => ""
        }
        block match {
          case StructuredText.Block.Heading(text, spans, level, _) => s"""<h$level$cls>${StructuredText.asHtml(text, spans, linkResolver, htmlSerializer)}</h$level>"""
          case StructuredText.Block.Paragraph(text, spans, _)      => s"""<p$cls>${StructuredText.asHtml(text, spans, linkResolver, htmlSerializer)}</p>"""
          case StructuredText.Block.Preformatted(text, spans, _)   => s"""<pre$cls>${StructuredText.asHtml(text, spans, linkResolver, htmlSerializer)}</pre>"""
          case StructuredText.Block.ListItem(text, spans, _, _)    => s"""<li$cls>${StructuredText.asHtml(text, spans, linkResolver, htmlSerializer)}</li>"""
          case StructuredText.Block.Image(view, Some(link: DocumentLink), _)     => s"""<p$cls><a href="$linkResolver(link)">${view.asHtml}</a></p>"""
          case StructuredText.Block.Image(view, Some(link: WebLink), _)     => s"""<p$cls><a href="${link.url}">${view.asHtml}</a></p>"""
          case StructuredText.Block.Image(view, Some(link: MediaLink), _)     => s"""<p$cls><a href="${link.url}">${view.asHtml}</a></p>"""
          case StructuredText.Block.Image(view, _, _)              => s"""<p$cls>${view.asHtml}</p>"""
          case StructuredText.Block.Embed(obj, _)                  => obj.asHtml
        }
      }

      case class Heading(text: String, spans: Seq[Span], level: Int, label: Option[String]) extends Text

      object Heading {
        implicit def reader(level: Int): Reads[Heading] = (
          (__ \ "text").read[String] and
            (__ \ "spans").read(Reads.seq(Span.reader.map(Option.apply).orElse(Reads.pure(None))).map(_.collect { case Some(span) => span })) and
            (__ \ "label").readNullable[String] tupled
        ).map {
            case (content, spans, label) => Heading(content, spans, level, label)
          }
      }

      case class Paragraph(text: String, spans: Seq[Span], label: Option[String]) extends Text

      object Paragraph {
        implicit val reader: Reads[Paragraph] = (
          (__ \ "text").read[String] and
            (__ \ "spans").read(Reads.seq(Span.reader.map(Option.apply).orElse(Reads.pure(None))).map(_.collect { case Some(span) => span })) and
            (__ \ "label").readNullable[String] tupled
        ).map {
            case (content, spans, label) => Paragraph(content, spans, label)
          }
      }

      case class Preformatted(text: String, spans: Seq[Span], label: Option[String]) extends Text

      object Preformatted {
        implicit val reader: Reads[Preformatted] = (
          (__ \ "text").read[String] and
            (__ \ "spans").read(Reads.seq(Span.reader.map(Option.apply).orElse(Reads.pure(None))).map(_.collect { case Some(span) => span })) and
            (__ \ "label").readNullable[String] tupled
        ).map {
            case (content, spans, label) => Preformatted(content, spans, label)
          }
      }

      case class ListItem(text: String, spans: Seq[Span], ordered: Boolean, label: Option[String]) extends Text

      object ListItem {
        implicit def reader(ordered: Boolean): Reads[ListItem] = (
          (__ \ "text").read[String] and
            (__ \ "spans").read(Reads.seq(Span.reader.map(Option.apply).orElse(Reads.pure(None))).map(_.collect { case Some(span) => span })) and
            (__ \ "label").readNullable[String] tupled
        ).map {
            case (content, spans, label) => ListItem(content, spans, ordered, label)
          }
      }

      case class Image(view: Fragment.Image.View, linkTo: Option[Link], label: Option[String]) extends Block {
        def url = view.url
        def width = view.width
        def height = view.height
      }

      object Image {
        implicit val reader: Reads[Image] = (
          (__ \ "label").readNullable[String] and
          (__ \ "linkTo").readNullable[Link] and
          __.read[Fragment.Image.View] tupled).map {
          case (label, linkTo, view) => Image(view, linkTo, label)
        }
      }

      case class Embed(obj: Fragment.Embed, label: Option[String]) extends Block

      implicit val reader: Reads[Block] = (__ \ "type").read[String].flatMap[Block] {

        case "heading1" => __.read(Heading.reader(1)).map(identity[Block])
        case "heading2" => __.read(Heading.reader(2)).map(identity[Block])
        case "heading3" => __.read(Heading.reader(3)).map(identity[Block])
        case "heading4" => __.read(Heading.reader(4)).map(identity[Block])
        case "paragraph" => __.read(Paragraph.reader).map(identity[Block])
        case "preformatted" => __.read(Preformatted.reader).map(identity[Block])
        case "list-item" => __.read(ListItem.reader(ordered = false)).map(identity[Block])
        case "o-list-item" => __.read(ListItem.reader(ordered = true)).map(identity[Block])
        case "image" => __.read(Image.reader).map(identity[Block])
        case "embed" => ((__ \ "label").readNullable[String] and __.read[Fragment.Embed] tupled).map {
          case (label, obj) => Embed(obj, label): Block
        }
        case t => Reads(json => JsError(s"Unsupported block type $t"))
      }

    }

    implicit val reader: Reads[StructuredText] = __.read(Reads.seq(Block.reader.map(Option(_)).orElse(implicitly[Reads[JsValue]].map(_ => None)))).map(_.flatten).map {
      case blocks => StructuredText(blocks)
    }

  }

}
