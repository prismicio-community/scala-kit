package com.zenexity.wroom.client

import scala.util._
import scala.concurrent._

import org.joda.time._

import play.api.libs.json._
import play.api.libs.functional.syntax._

object `package` {

  type LinkResolver = (Fragment.Link => LinkDestination)

  val DefaultLinkResolver: LinkResolver = { link =>
    link match {
      case Fragment.WebLink(url, _) => LinkDestination(url)
      case Fragment.MediaLink(url, _, _, _) => LinkDestination(url)
      case Fragment.DocumentLink(id, typ, tags, slug) => LinkDestination(s"""#$id:$typ:${tags.mkString(",")}:$slug""")
    }
  }

}

case class Ref(
  ref: String,
  label: String,
  isMasterRef: Boolean = false,
  scheduledAt: Option[DateTime] = None
)

object Ref {

  implicit val reader = (
    (__ \ "ref").read[String] and
    (__ \ "label").read[String] and
    ((__ \ "isMasterRef").read[Boolean] orElse Reads.pure(false)) and
    (__ \ "scheduledAt").readNullable[DateTime]
  )(Ref.apply _)

}

case class Field(`type`: String, default: Option[String])

object Field {
  implicit val reader = Json.reads[Field]
}

case class Form(
  name: Option[String],
  method: String,
  rel: Option[String],
  enctype: String,
  action: String,
  fields: Map[String, Field]
) {

  def defaultData: Map[String,String] = {
    fields.mapValues(_.default).collect {
      case (key, Some(value)) => (key, value)
    }
  } 

}

object Form {
  implicit val reader = Json.reads[Form]
}

case class ApiData(
  val refs: Seq[Ref],
  val bookmarks: Map[String, String],
  val types: Map[String, String],
  val tags: Seq[String],
  val forms: Map[String, Form]
)

object ApiData {
  
  implicit val reader = (
    (__ \ 'refs).read[Seq[Ref]] and
    (__ \ 'bookmarks).read[Map[String, String]] and
    (__ \ 'types).read[Map[String, String]] and
    (__ \ 'tags).read[Seq[String]] and
    (__ \ 'forms).read[Map[String, Form]]
  )(ApiData.apply _)

}

sealed trait Fragment {
  def asHtml(linkResolver: LinkResolver = DefaultLinkResolver): String = ""
}

case class LinkDestination(url: String, target: Option[String] = None)

object Fragment {

  sealed trait Link extends Fragment

  case class WebLink(url: String, contentType: Option[String] = None) extends Link {
    override def asHtml(linkResolver: LinkResolver = DefaultLinkResolver): String = {
      val destination = linkResolver(this)
      val target = destination.target.map(t => s"""target="$t" """)
      s"""<a ${target}href="${destination.url}}">$url</a>"""
    }
  }

  case class MediaLink(url: String, contentType: String, size: Long, filename: String) extends Link {
    override def asHtml(linkResolver: LinkResolver = DefaultLinkResolver): String = {
      val destination = linkResolver(this)
      val target = destination.target.map(t => s"""target="$t" """)
      s"""<a ${target}href="${destination.url}}">$filename</a>"""
    }
  }

  case class DocumentLink(id: String, typ: String, tags: Seq[String], slug: String) extends Link {
    override def asHtml(linkResolver: LinkResolver = DefaultLinkResolver): String = {
      val destination = linkResolver(this)
      val target = destination.target.map(t => s"""target="$t" """)
      s"""<a ${target}href="${destination.url}}">$id/$slug</a>"""
    }
  }

  // ------------------

  case class Number(value: Double) extends Fragment {  
    def asInt = value.toInt
    def asText(pattern: String) = new java.text.DecimalFormat(pattern).format(value)

    override def asHtml(linkResolver: LinkResolver = DefaultLinkResolver): String = {
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

    override def asHtml(linkResolver: LinkResolver = DefaultLinkResolver): String = {
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

    def getView(key: String): Option[Image.View] = key match {
      case "main" => Some(main)
      case _ => views.get(key)
    }

    override def asHtml(linkResolver: LinkResolver = DefaultLinkResolver): String = main.asHtml()

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

    override def asHtml(linkResolver: LinkResolver = DefaultLinkResolver): String = {
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

      implicit val reader: Reads[Span] =
        (
          (__ \ 'type).read[String] and
          (__ \ 'start).read[Int] and
          (__ \ 'end).read[Int] and
          __.read[JsObject]
        ).tupled.map {
          case (typ, start, end, obj) => typ match {
            case "strong" => Strong(start, end)
            case "em" => Em(start, end)
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
        implicit def reader(level: Int): Reads[Heading] = (
          (__ \ "text").read[String] and
          (__ \ "spans").read[Seq[Span]] tupled
        ).map {
          case (content, spans) => Heading(level, content, spans)
        }
      }
      
      case class Paragraph(text: String, spans: Seq[Span]) extends Text

      object Paragraph {
        implicit val reader: Reads[Paragraph] = (
          (__ \ "text").read[String] and
          (__ \ "spans").read[Seq[Span]] tupled
        ).map {
          case (content, spans) => Paragraph(content, spans)
        }
      }

      case class Image(view: Fragment.Image.View) extends Block {
        def url = view.url
        def width = view.width
        def height = view.height
      }

      implicit val reader: Reads[Block] = (
        (__ \ "type").read[String].flatMap[Block] { 

          case "heading1"  => __.read(Heading.reader(1)).map(identity[Block])
          case "heading2"  => __.read(Heading.reader(2)).map(identity[Block])
          case "heading3"  => __.read(Heading.reader(3)).map(identity[Block])
          case "heading4"  => __.read(Heading.reader(4)).map(identity[Block])
          case "paragraph" => __.read[Paragraph].map(identity[Block])
          case "image"     => __.read[Fragment.Image.View].map(view => Image(view):Block)

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


case class Document(
  id: String,
  typ: String,
  href: String,
  tags: Seq[String],
  slugs: Seq[String],
  fragments: Map[String, Fragment]
) {

  def slug: String = slugs.headOption.getOrElse("-")

  def apply(field: String): Fragment = fragments(field)

  def get(field: String): Option[Fragment] = fragments.get(field)

  def getImage(field: String): Option[Fragment.Image] = get(field).flatMap {
    case a: Fragment.Image => Some(a)
    case a: Fragment.StructuredText => a.blocks.collectFirst { case b: Fragment.StructuredText.Block.Image => b.view }.map(v => Fragment.Image(v))
    case _ => None
  }

  def getImage(field: String, view: String): Option[Fragment.Image.View] = get(field).flatMap {
    case a: Fragment.Image => a.getView(view)
    case a: Fragment.StructuredText if view == "main" => getImage(field).map(_.main)
    case _ => None
  }

  def getStructuredText(field: String): Option[Fragment.StructuredText] = get(field).flatMap {
    case a: Fragment.StructuredText => Some(a)
    case _ => None
  }

  def getHtml(field: String, linkResolver: LinkResolver = DefaultLinkResolver): Option[String] = {
    get(field).map(_.asHtml(linkResolver))
  }

  def getText(field: String): Option[String] = get(field).flatMap {
    case a: Fragment.StructuredText => Some(a.blocks.collect { case b: Fragment.StructuredText.Block.Text => b.text }.mkString("\n")).filterNot(_.isEmpty)
    case a: Fragment.Number => Some(a.value.toString)
    case a: Fragment.Color => Some(a.hex)
    case _ => None
  }

  def getColor(field: String): Option[Fragment.Color] = get(field).flatMap {
    case a: Fragment.Color => Some(a)
    case _ => None
  }

  def getNumber(field: String): Option[Fragment.Number] = get(field).flatMap {
    case a: Fragment.Number => Some(a)
    case _ => None
  }

  def getNumber(field: String, pattern: String): Option[String] = getNumber(field).map(_.asText(pattern))

}

object Document {
  implicit val reader = (
    (__ \ "id").read[String] and
    (__ \ "href").read[String] and
    (__ \ "tags").read[Seq[String]] and
    (__ \ "slugs").read[Seq[String]] and
    (__ \ "type").read[String].flatMap[(String,Map[String,Fragment])] { typ =>
      (__ \ "data" \ typ).read[JsObject].map { data =>
        data.fields.map { 
          case (key, jsvalue) => 
            (jsvalue \ "type").asOpt[String].flatMap {

              case "Image" => Some(Fragment.Image.reader.map(identity[Fragment]))
              case "Color" => Some(Fragment.Color.reader.map(identity[Fragment]))
              case "Number" => Some(Fragment.Number.reader.map(identity[Fragment]))
              case "StructuredText" => Some(Fragment.StructuredText.reader.map(identity[Fragment]))

              case t => None
            }.flatMap(_.reads(jsvalue \ "value").asOpt).toList.map(fragment => (s"$typ.$key", fragment))
        }.flatten.toMap
      }.map(data => (typ,data))
    }
  )((id, href, tags, slugs, typAndData) => Document(id, typAndData._1, href, tags, slugs, typAndData._2))

}
