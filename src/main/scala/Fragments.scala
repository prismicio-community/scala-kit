package io.prismic

import org.joda.time._

import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.util._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

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

  case class MediaLink(url: String, contentType: String, size: Long, filename: String) extends Link {
    def asHtml: String = s"""<a href="$url">$filename</a>"""
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
      case _ => false
    } 

    def asRGB(hex: String): (Int, Int, Int) = hex match {
      case HexColor(r, g, b) => (Integer.parseInt(r, 16), Integer.parseInt(g, 16), Integer.parseInt(b, 16))
      case _ => (0,0,0)
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

  case class Image(main: Image.View, views: Map[String, Image.View] = Map.empty) extends Fragment {

    def getView(key: String): Option[Image.View] = key.toLowerCase match {
      case "main" => Some(main)
      case _ => views.get(key)
    }

    def asHtml: String = main.asHtml

  }

  object Image {

    case class View(url: String, width: Int, height: Int) {
      def ratio = width / height
      def asHtml: String = s"""<img src="${url}" width="${width}" height="${height}">"""
    }

    implicit val viewReader: Reads[View] = 
      (
        (__ \ 'url).read[String] and
        (__ \ 'dimensions).read(
          (__ \ 'width).read[Int] and
          (__ \ 'height).read[Int] tupled
        )
      ).tupled.map {
        case (url, (width, height)) => View(url, width, height)
      }

    implicit val reader: Reads[Image] =
      (
        (__ \ 'main).read[View] and
        (__ \ 'views).read[Map[String,View]]
      ).tupled.map {
        case (main, views) => Image(main, views)
      }

  }

  case class StructuredText(blocks: Seq[StructuredText.Block]) extends Fragment {

    def getTitle: Option[StructuredText.Block.Heading] = blocks.collectFirst {
      case h: StructuredText.Block.Heading => h
    }

    def getFirstParagraph: Option[StructuredText.Block.Paragraph] = blocks.collectFirst {
      case p: StructuredText.Block.Paragraph => p
    }

    def getFirstImage: Option[StructuredText.Block.Image] = blocks.collectFirst {
      case i: StructuredText.Block.Image => i
    }

    def asHtml(linkResolver: DocumentLinkResolver): String = {
      StructuredText.asHtml(blocks, linkResolver)
    }

  }

  // ------------------

  object StructuredText {

    def asHtml(blocks: Seq[Block], linkResolver: DocumentLinkResolver): String = {
      case class Group(htmlTag: Option[String], blocks: Seq[Block])

      val grouped = blocks.foldLeft(List.empty[Group]) {
        case ((group @ Group(Some("ul"), _)) :: rest, block @ StructuredText.Block.ListItem(text, spans, false)) => group.copy(blocks = group.blocks :+ block) +: rest
        case ((group @ Group(Some("ol"), _)) :: rest, block @ StructuredText.Block.ListItem(text, spans, true)) => group.copy(blocks = group.blocks :+ block) +: rest
        case (groups, block @ StructuredText.Block.ListItem(text, spans, false)) => Group(Some("ul"), Seq(block)) +: groups
        case (groups, block @ StructuredText.Block.ListItem(text, spans, true)) => groups :+ Group(Some("ol"), Seq(block))
        case (groups, block) => Group(None, Seq(block)) +: groups
      }.reverse

      grouped.flatMap {
        case Group(Some(tag), blocks) => (s"<$tag>") +: blocks.map(block => asHtml(block, linkResolver)) :+ (s"</$tag>")
        case Group(None, blocks) => blocks.map(block => asHtml(block, linkResolver))
      }.mkString("\n\n")
    }

    def asHtml(block: Block, linkResolver: DocumentLinkResolver): String = {
      block match {
        case StructuredText.Block.Heading(text, spans, level) => s"""<h$level>${asHtml(text, spans, linkResolver)}</h$level>"""
        case StructuredText.Block.Paragraph(text, spans) => s"""<p>${asHtml(text, spans, linkResolver)}</p>""" 
        case StructuredText.Block.Preformatted(text, spans) => s"""<pre>${asHtml(text, spans, linkResolver)}</pre>""" 
        case StructuredText.Block.ListItem(text, spans, _) => s"""<li>${asHtml(text, spans, linkResolver)}</li>"""
        case StructuredText.Block.Image(view) => s"""<p>${view.asHtml}</p>"""
        case StructuredText.Block.Embed(obj) => obj.asHtml
      }
    }

    def asHtml(text: String, spans: Seq[Span], linkResolver: DocumentLinkResolver): String = {

      def escape(character: String): String = {
        character.replace("<", "&lt;").replace("\n", "<br>")
      }

      def writeTag(span: Span, opening: Boolean): String = {
        span match {
          case Span.Em(_, _) => if(opening) "<em>" else "</em>"
          case Span.Strong(_, _) => if(opening) "<strong>" else "</strong>"
          case Span.Hyperlink(_, _, link:DocumentLink) => if(opening) s"""<a href="${linkResolver(link)}">""" else "</a>"
          case Span.Hyperlink(_, _, link:MediaLink) => if(opening) s"""<a href="${link.url}">""" else "</a>"
          case Span.Hyperlink(_, _, link:WebLink) => if(opening) s"""<a href="${link.url}">""" else "</a>"
          case _ => if(opening) "<span>" else "</span>"
        }
      }

      def writeHtml(endingsToApply: Seq[Span], startingsToApply: Seq[Span]): String = {
        endingsToApply.map(e => writeTag(e, opening = false)).mkString + startingsToApply.map(s => writeTag(s, opening = true)).mkString
      }

      import scala.collection.mutable.ListBuffer

      @scala.annotation.tailrec
      def step(in: Seq[(Char, Int)], startings: Seq[Span], endings: Seq[Span] = Nil, html: ListBuffer[String] = ListBuffer()): String = {
        val nextOp = (startings.headOption.map(_.start).toList ++ endings.headOption.toList.map(_.end)).reduceOption(Math.min)
        in match {
          case ((_, pos) :: tail) if !nextOp.exists(_ == pos) => {
            val (done,toDo) = in.toList.span(i => ! nextOp.exists(i._2 == _))
            step(toDo, startings, endings, html += escape(done.map(_._1).mkString))
          }
          case (current, pos) :: tail => {
            val (endingsToApply, othersEnding) = endings.span(_.end == pos)
            val (startingsToApply, othersStarting) = startings.span(_.start == pos)
            val applied = writeHtml(endingsToApply, startingsToApply) + escape(current.toString)
            val moreEndings = startingsToApply.reverse
            val newEndings = moreEndings ++ othersEnding
            step(tail, othersStarting, newEndings, html += applied)
          }
          case Nil => html.mkString + writeHtml(endings, Seq.empty).mkString
        }
      }

      step(text.toList.zipWithIndex, spans.sortBy(_.start))
    }

    sealed trait Span {
      def start: Int
      def end: Int
    }

    object Span {

      case class Em(start: Int, end: Int) extends Span
      case class Strong(start: Int, end: Int) extends Span
      case class Hyperlink(start: Int, end: Int, link: Link) extends Span

      implicit val reader: Reads[Span] =
        (
          (__ \ 'type).read[String] and
          (__ \ 'start).read[Int] and
          (__ \ 'end).read[Int] and
          (__ \ 'data).readNullable[JsObject].map(_.getOrElse(Json.obj()))
        ).tupled.flatMap {
          case (typ, start, end, data) => typ match {
            case "strong" => Reads.pure(Strong(start, end))
            case "em" => Reads.pure(Em(start, end))
            case "hyperlink" if (data \ "type").asOpt[String].exists(_ == "Link.web") => (__ \ "data" \ "value").read(WebLink.reader).map(link => Hyperlink(start, end, link))
            case "hyperlink" if (data \ "type").asOpt[String].exists(_ == "Link.document") => (__ \ "data" \ "value").read(DocumentLink.reader).map(link => Hyperlink(start, end, link)) 
            case t => Reads(json => JsError(s"Unsupported span type $t"))
          }
        }
    }

    sealed trait Block

    object Block {

      sealed trait Text extends Block {
        def text: String
        def spans: Seq[Span]
      }

      case class Heading(text: String, spans: Seq[Span], level: Int) extends Text

      object Heading {
        implicit def reader(level: Int): Reads[Heading] = (
          (__ \ "text").read[String] and
          (__ \ "spans").read(Reads.seq(Span.reader.map(Option.apply _).orElse(Reads.pure(None))).map(_.collect { case Some(span) => span })) tupled
        ).map {
          case (content, spans) => Heading(content, spans, level)
        }
      }
      
      case class Paragraph(text: String, spans: Seq[Span]) extends Text

      object Paragraph {
        implicit val reader: Reads[Paragraph] = (
          (__ \ "text").read[String] and
          (__ \ "spans").read(Reads.seq(Span.reader.map(Option.apply _).orElse(Reads.pure(None))).map(_.collect { case Some(span) => span })) tupled
        ).map {
          case (content, spans) => Paragraph(content, spans)
        }
      }

      case class Preformatted(text: String, spans: Seq[Span]) extends Text

      object Preformatted {
        implicit val reader: Reads[Preformatted] = (
          (__ \ "text").read[String] and
          (__ \ "spans").read(Reads.seq(Span.reader.map(Option.apply _).orElse(Reads.pure(None))).map(_.collect { case Some(span) => span })) tupled
        ).map {
          case (content, spans) => Preformatted(content, spans)
        }
      }

      case class ListItem(text: String, spans: Seq[Span], ordered: Boolean) extends Text

      object ListItem {
        implicit def reader(ordered: Boolean): Reads[ListItem] = (
          (__ \ "text").read[String] and
          (__ \ "spans").read(Reads.seq(Span.reader.map(Option.apply _).orElse(Reads.pure(None))).map(_.collect { case Some(span) => span })) tupled
        ).map {
          case (content, spans) => ListItem(content, spans, ordered)
        }
      }

      case class Image(view: Fragment.Image.View) extends Block {
        def url = view.url
        def width = view.width
        def height = view.height
      }

      case class Embed(obj: Fragment.Embed) extends Block

      implicit val reader: Reads[Block] = (
        (__ \ "type").read[String].flatMap[Block] { 

          case "heading1"     => __.read(Heading.reader(1)).map(identity[Block])
          case "heading2"     => __.read(Heading.reader(2)).map(identity[Block])
          case "heading3"     => __.read(Heading.reader(3)).map(identity[Block])
          case "heading4"     => __.read(Heading.reader(4)).map(identity[Block])
          case "paragraph"    => __.read(Paragraph.reader).map(identity[Block])
          case "preformatted" => __.read(Preformatted.reader).map(identity[Block])
          case "list-item"    => __.read(ListItem.reader(ordered = false)).map(identity[Block])
          case "image"        => __.read[Fragment.Image.View].map(view => Image(view):Block)
          case "embed"        => __.read[Fragment.Embed].map(obj => Embed(obj):Block)

          case t => Reads(json => JsError(s"Unsupported block type $t"))
        }
      )

    }

    implicit val reader: Reads[StructuredText] = (
      __.read(Reads.seq(Block.reader.map(Option(_)).orElse(implicitly[Reads[JsValue]].map(_ => None)))).map(_.flatten).map {
        case blocks => StructuredText(blocks)
      }
    )

  }

}