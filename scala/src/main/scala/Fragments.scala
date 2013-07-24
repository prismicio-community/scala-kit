package io.prismic

import org.joda.time._

import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.util._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

object `package` {

  type LinkResolver = (Fragment.Link => LinkDestination)

}

sealed trait Fragment {
  def asHtml(linkResolver: LinkResolver): String = ""
}

case class LinkDestination(url: String, target: Option[String] = None)

object Fragment {

  sealed trait Link extends Fragment

  case class WebLink(url: String, contentType: Option[String] = None) extends Link {
    override def asHtml(linkResolver: LinkResolver): String = {
      val destination = linkResolver(this)
      val target = destination.target.map(t => s"""target="$t" """)
      s"""<a ${target}href="${destination.url}}">$url</a>"""
    }
  }

  object WebLink {

    implicit val reader: Reads[WebLink] = {
      (__ \ "url").read[String].map {
        case url => WebLink(url)
      }
    }

  }

  case class MediaLink(url: String, contentType: String, size: Long, filename: String) extends Link {
    override def asHtml(linkResolver: LinkResolver): String = {
      val destination = linkResolver(this)
      val target = destination.target.map(t => s"""target="$t" """)
      s"""<a ${target}href="${destination.url}}">$filename</a>"""
    }
  }

  case class DocumentLink(id: String, typ: String, tags: Seq[String], slug: String, bookmark: Option[String], isBroken: Boolean) extends Link {
    override def asHtml(linkResolver: LinkResolver): String = {
      val destination = linkResolver(this)
      val target = destination.target.map(t => s"""target="$t" """)
      s"""<a ${target}href="${destination.url}}">$id/$slug</a>"""
    }
  }

  object DocumentLink {

    implicit def reader(apiData: ApiData): Reads[DocumentLink] = {
      (
        (__ \ "document").read(
          (__ \ "id").read[String] and
          (__ \ "type").read[String] and
          (__ \ "tags").readNullable[Seq[String]].map(_.getOrElse(Nil)) and
          (__ \ "slug").read[String] tupled
        ) and
        (__ \ "isBroken").readNullable[Boolean].map(_.getOrElse(false))
      ).tupled.map(link => DocumentLink(link._1._1, link._1._2, link._1._3, link._1._4, apiData.bookmarks.find(_._2 == link._1._1).map(_._1), link._2))
    }

  }

  // ------------------

  case class Text(value: String) extends Fragment {  
    override def asHtml(linkResolver: LinkResolver): String = {
      s"""<span class="text">$value</span>"""
    }
  }

  object Text {
    implicit val reader: Reads[Text] = {
      Reads(v => v.asOpt[String].map(d => JsSuccess(Text(d))).getOrElse(JsError(s"Invalid text value $v")))
    }
  }

  // ------------------

  case class Date(value: DateTime) extends Fragment {  
    def asText(pattern: String) = value.toString(pattern)

    override def asHtml(linkResolver: LinkResolver): String = {
      s"""<time>$value</time>"""
    }
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

    override def asHtml(linkResolver: LinkResolver): String = {
      s"""<span class="number">$value</span>"""
    }
  }

  object Number {
    implicit val reader: Reads[Number] = {
      Reads(v => v.asOpt[Double].map(d => JsSuccess(Number(d))).getOrElse(JsError(s"Invalid number value $v")))
    }
  }

  // ------------------

  case class Color(hex: String) extends Fragment {
    def asRGB = Color.asRGB(hex)

    override def asHtml(linkResolver: LinkResolver): String = {
      s"""<span class="color">$hex</span>"""
    }
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

  case class Image(main: Image.View, views: Map[String, Image.View] = Map.empty) extends Fragment {

    def getView(key: String): Option[Image.View] = key.toLowerCase match {
      case "main" => Some(main)
      case _ => views.get(key)
    }

    override def asHtml(linkResolver: LinkResolver): String = main.asHtml()

  }

  object Image {

    case class View(url: String, width: Int, height: Int) {
      def ratio = width / height

      def asHtml(): String = {
        s"""<img src="${url}" width="${width}" height="${height}">"""
      }
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

    override def asHtml(linkResolver: LinkResolver): String = {
      blocks.map(block => StructuredText.asHtml(block, linkResolver)).mkString("\n\n")
    }

  }

  // ------------------

  object StructuredText {

    def asHtml(block: Block, linkResolver: LinkResolver): String = {
      block match {
        case StructuredText.Block.Heading(x, text, spans) => s"""<h$x>${asHtml(text, spans, linkResolver)}</h$x>"""
        case StructuredText.Block.Paragraph(text, spans) => s"""<p>${asHtml(text, spans, linkResolver)}</p>""" 
        case StructuredText.Block.Image(view) => s"""<p>${view.asHtml()}</p>"""
      }
    }

    def asHtml(text: String, spans: Seq[Span], linkResolver: LinkResolver): String = {

      def escape(character: String): String = {
        character.replace("<", "&lt;").replace("\n", "<br>")
      }

      def writeTag(span: Span, opening: Boolean): String = {
        span match {
          case em: Span.Em => if(opening) "<em>" else "</em>"
          case strong: Span.Strong => if(opening) "<strong>" else "</strong>"
          case link: Span.Hyperlink => if(opening) s"""<a href="${linkResolver(link.link).url}">""" else "</a>"
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

      implicit def reader(apiData: ApiData): Reads[Span] =
        (
          (__ \ 'type).read[String] and
          (__ \ 'start).read[Int] and
          (__ \ 'end).read[Int] and
          (__ \ 'data).readNullable[JsObject].map(_.getOrElse(Json.obj()))
        ).tupled.flatMap {
          case (typ, start, end, data) => typ match {
            case "strong" => Reads.pure(Strong(start, end))
            case "em" => Reads.pure(Em(start, end))
            case "hyperlink" if (data \ "type").as[String] == "Link.web" => (__ \ "data" \ "value").read(WebLink.reader).map(link => Hyperlink(start, end, link))
            case "hyperlink" if (data \ "type").as[String] == "Link.document" => (__ \ "data" \ "value").read(DocumentLink.reader(apiData)).map(link => Hyperlink(start, end, link)) 
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

      case class Heading(level: Int, text: String, spans: Seq[Span]) extends Text

      object Heading {
        implicit def reader(apiData: ApiData, level: Int): Reads[Heading] = (
          (__ \ "text").read[String] and
          (__ \ "spans").read(Reads.seq(Span.reader(apiData).map(Option.apply _).orElse(Reads.pure(None))).map(_.collect { case Some(span) => span })) tupled
        ).map {
          case (content, spans) => Heading(level, content, spans)
        }
      }
      
      case class Paragraph(text: String, spans: Seq[Span]) extends Text

      object Paragraph {
        implicit def reader(apiData: ApiData): Reads[Paragraph] = (
          (__ \ "text").read[String] and
          (__ \ "spans").read(Reads.seq(Span.reader(apiData).map(Option.apply _).orElse(Reads.pure(None))).map(_.collect { case Some(span) => span })) tupled
        ).map {
          case (content, spans) => Paragraph(content, spans)
        }
      }

      case class Image(view: Fragment.Image.View) extends Block {
        def url = view.url
        def width = view.width
        def height = view.height
      }

      implicit def reader(apiData: ApiData): Reads[Block] = (
        (__ \ "type").read[String].flatMap[Block] { 

          case "heading1"  => __.read(Heading.reader(apiData, 1)).map(identity[Block])
          case "heading2"  => __.read(Heading.reader(apiData, 2)).map(identity[Block])
          case "heading3"  => __.read(Heading.reader(apiData, 3)).map(identity[Block])
          case "heading4"  => __.read(Heading.reader(apiData, 4)).map(identity[Block])
          case "paragraph" => __.read(Paragraph.reader(apiData)).map(identity[Block])
          case "image"     => __.read[Fragment.Image.View].map(view => Image(view):Block)

          case t => Reads(json => JsError(s"Unsupported block type $t"))
        }
      )

    }

    implicit def reader(apiData: ApiData): Reads[StructuredText] = (
      __.read(Reads.seq(Block.reader(apiData).map(Option(_)).orElse(implicitly[Reads[JsValue]].map(_ => None)))).map(_.flatten).map {
        case blocks => StructuredText(blocks)
      }
    )

  }

}