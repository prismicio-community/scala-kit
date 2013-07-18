package com.zenexity.wroom.client

import scala.util._
import scala.concurrent._

import org.joda.time._

import play.api.libs.json._
import play.api.libs.functional.syntax._

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
  def asHtml: String = ""
}


object Fragment {

  case class Number(value: Double) extends Fragment {
    def asInt = value.toInt
    def asText(pattern: String) = new java.text.DecimalFormat(pattern).format(value)
  }

  object Number {

    implicit val reader: Reads[Number] = {
      Reads(v => v.asOpt[Double].map(d => JsSuccess(Number(d))).getOrElse(JsError(s"Invalid number value $v")))
    }

  }

  // ------------------

  case class Color(hex: String) extends Fragment {
    def asRGB = Color.asRGB(hex)
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

  }

  object Image {

    case class View(url: String, width: Int, height: Int) {
      def ratio = width / height
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

  }

  // ------------------

  object StructuredText {

    sealed trait Span {
      def start: Int
      def end: Int
      def typ: String
    }

    object Span {

      case class Em(start: Int, end: Int) extends Span {
        override val typ = "em"
      }
      object Em {
        val writer = Json.writes[Em]
      }

      case class Strong(start: Int, end: Int) extends Span {
        override val typ = "strong"
      }
      object Strong {
        val writer = Json.writes[Strong]
      }

      trait HyperLink extends Span {
        override val typ = "hyperlink"

        def preview: Option[HyperLink.LinkPreview]
        def asUrl: String
      }

      object HyperLink {
        case class LinkPreview(title: Option[String], image: Option[String])
        object LinkPreview {
          implicit val format = Json.format[LinkPreview]
        }

        case class DocumentLink(start: Int, end: Int, id: String, mask: String, preview: Option[LinkPreview])
        extends HyperLink {
          def asUrl = s"wio://documents/$id"
        }
        object DocumentLink {
          implicit val reader = Json.reads[DocumentLink]
          val writer = Json.writes[DocumentLink]
        }

        sealed trait MediaLink extends HyperLink {
          def id: String
          def asUrl = s"wio://medias/$id"
        }

        case class FileLink(start: Int, end: Int, id: String, url: String, name: String, kind: String, date: String, size: String, preview: Option[LinkPreview])
        extends MediaLink

        object FileLink {
          implicit val reader = Json.reads[FileLink]
          val writer = Json.writes[FileLink]
        }

        case class ImageLink(start: Int, end: Int, id: String, url: String, date: String, height: String, width: String, size: String, name: String, kind: String, preview: Option[LinkPreview])
        extends MediaLink

        object ImageLink {
          implicit val reader = Json.reads[ImageLink]
          val writer = Json.writes[ImageLink]
        }

        case class ExternalLink(start: Int, end: Int, url: String, preview: Option[LinkPreview]) extends HyperLink {
          def asUrl = url
        }
        object ExternalLink {
          implicit val reader = Json.reads[ExternalLink]
          val writer = Json.writes[ExternalLink]
        }

        case class EmptyLink(start: Int, end: Int) extends HyperLink {
          def asUrl = ""
          def preview = None
        }
        object EmptyLink{
          implicit val reader = Json.reads[EmptyLink]
          val writer = Json.writes[EmptyLink]
        }

        object MediaLink {
          implicit val reader: Reads[MediaLink] = 
            __.read[FileLink].map(e => e:MediaLink) or 
            __.read[ImageLink].map(e => e:MediaLink)

          val writer = Writes[MediaLink] { 
            case a:FileLink => Json.toJson[FileLink](a)(FileLink.writer)
            case a:ImageLink => Json.toJson[ImageLink](a)(ImageLink.writer)
          }
        }

        implicit val reader: Reads[HyperLink] = 
          __.read[DocumentLink].map(e => e:HyperLink) or 
          __.read[MediaLink].map(e => e:HyperLink) or 
          __.read[ExternalLink].map(e => e:HyperLink) or
          __.read[EmptyLink].map(e => e:HyperLink)
        
        implicit val writer = Writes[HyperLink]{
          case a:DocumentLink => Json.toJson(a)(DocumentLink.writer)
          case a:MediaLink => Json.toJson(a)(MediaLink.writer)
          case a:ExternalLink => Json.toJson(a)(ExternalLink.writer)
          case a:EmptyLink => Json.toJson(a)(EmptyLink.writer)
        }
      }

      implicit val reader: Reads[Span] =
        (
          (__ \ 'type).read[String] and
          (__ \ 'start).read[Int] and
          (__ \ 'end).read[Int] and
          __.read[JsObject]
        ).tupled.map{
          case (typ, start, end, obj) => typ match {
            case "em" => Em(start, end)
            case "strong" => Strong(start, end)
            case "hyperlink" => obj.as[HyperLink]
          }
        }
        implicit val writer = Writes[Span]{
          case a:Em => Json.toJson(a)(Em.writer)
          case a:Strong => Json.toJson(a)(Strong.writer)
          case a:HyperLink => Json.toJson(a)(HyperLink.writer)
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
