package com.zenexity.wroom.client

import scala.concurrent._
import org.joda.time._

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Ref(
  ref: String,
  label: String,
  isMasterRef: Boolean = false,
  scheduled: Option[DateTime] = None
)

object Ref {
  implicit val reader = (
    (__ \ "ref").read[String] and
    (__ \ "label").read[String] and
    ((__ \ "master-ref").read[Boolean] orElse Reads.pure(false)) and
    (__ \ "scheduled").readNullable[DateTime]
  )(Ref.apply _)

  implicit val writer = (
    (__ \ "ref").write[String] and
    (__ \ "label").write[String] and
    (__ \ "master-ref").write[Boolean] and
    (__ \ "scheduled").writeNullable[DateTime]
  )(unlift(Ref.unapply))
}

sealed trait FieldDef {
  def typ: String
  def multiple: Boolean = false
}

case class SingleFieldDef(override val typ: String, default: Option[String]) extends FieldDef {
  override val multiple: Boolean = false
}
object SingleFieldDef {
  implicit val reader = (
    (__ \ "type").read[String] and
    //((__ \ "multiple").read[Boolean] orElse Reads.pure(false)) and
    (__ \ "default").readNullable[String]
  )(SingleFieldDef.apply _)

  val writer = (
    (__ \ "type").write[String] and
    (__ \ "multiple").write[Boolean] and
    (__ \ "default").writeNullable[String]
  )((field: SingleFieldDef) => (field.typ, field.multiple, field.default))

}
case class MultipleFieldDef(override val typ: String, default: Seq[String]) extends FieldDef {
  override val multiple: Boolean = true
}
object MultipleFieldDef {
  implicit val reader = (
    (__ \ "type").read[String] and
    //((__ \ "multiple").read[Boolean] orElse Reads.pure(false)) and
    ((__ \ "default").read[Seq[String]] orElse Reads.pure(Seq()))
  )(MultipleFieldDef.apply _)

  val writer = (
    (__ \ "type").write[String] and
    (__ \ "multiple").write[Boolean] and
    (__ \ "default").write[Seq[String]]
  )((field: MultipleFieldDef) => (field.typ, field.multiple, field.default))
}

object FieldDef {
  implicit val reader =
    (
      (__ \ 'multiple).read[Boolean] and
      __.json.pick
    ).tupled flatMap { case (multiple, js) => multiple match {
      case true   => Reads{ _ => Json.fromJson[MultipleFieldDef](js) } map { c => c:FieldDef }
      case false  => Reads{ _ => Json.fromJson[SingleFieldDef](js) } map { c => c:FieldDef }
    } }

  implicit val writer = Writes[FieldDef]{ field => field match {
    case s: SingleFieldDef   => Json.toJson[SingleFieldDef](s)(SingleFieldDef.writer)
    case s: MultipleFieldDef => Json.toJson[MultipleFieldDef](s)(MultipleFieldDef.writer)
  }}
}

case class Form(
  name: Option[String],
  method: String,
  rel: Option[String],
  enctype: String,
  action: String,
  fields: Map[String, FieldDef]
)
object Form {
  implicit val reader = Json.reads[Form]
  implicit val writer = Json.writes[Form]
}

case class ApiData(
  val refs: Seq[Ref],
  val bookmarks: Map[String, String],
  val types: Map[String, String],
  val tags: Seq[String],
  val forms: Map[String, Form]
)
object ApiData {
  //implicit val reader = Json.reads[ApiData]
  implicit val reader = (
    (__ \ 'refs).read[Seq[Ref]] and
    (__ \ 'bookmarks).read[Map[String, String]] and
    (__ \ 'types).read[Map[String, String]] and
    (__ \ 'tags).read[Seq[String]] and
    (__ \ 'forms).read[Map[String, Form]]
  )(ApiData.apply _)

  implicit val writer = Json.writes[ApiData]
}


sealed trait Fragment {
  
  def asHtml: String = ""
}


object Fragment{

  /**
   * Image
   */
  case class Image(main: Image.View, views: Map[String, Image.View]) extends Fragment {

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
        (__ \ 'thumbnails).read[Map[String,View]]
      ).tupled.map {
        case (main, thumbnails) => Image(main, thumbnails)
      }

  }

  case class StructuredText(blocks: Seq[StructuredText.Block]) extends Fragment {

    def getTitle: Option[StructuredText.Block.Heading] = blocks.collectFirst {
      case h: StructuredText.Block.Heading => h
    }

  }

  object StructuredText {

    sealed trait Span {
      def start: Int
      def end: Int
      def `type`: String
    }

    object Span {

      case class Em(start: Int, end: Int) extends Span {
        override val `type` = "em"
      }
      object Em {
        val writer = Json.writes[Em]
      }

      case class Strong(start: Int, end: Int) extends Span {
        override val `type` = "strong"
      }
      object Strong {
        val writer = Json.writes[Strong]
      }

      trait HyperLink extends Span {
        override val `type` = "hyperlink"

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

      sealed trait TextBlock extends Block {
        def text: String
        def spans: Seq[Span]
      }

      case class Heading(level: Int, text: String, spans: Seq[Span]) extends TextBlock

      object Heading {
        implicit def reader(level: Int): Reads[Heading] = (
          (__ \ "content").read[String] and
          (__ \ "meta").read[Seq[Span]] tupled
        ).map {
          case (content, spans) => Heading(level, content, spans)
        }
      }
      
      case class Paragraph(text: String, spans: Seq[Span]) extends TextBlock

      object Paragraph {
        implicit val reader: Reads[Paragraph] = (
          (__ \ "content").read[String] and
          (__ \ "meta").read[Seq[Span]] tupled
        ).map {
          case (content, spans) => Paragraph(content, spans)
        }
      }

      implicit val reader: Reads[Block] = (
        (__ \ "label").read[String].flatMap[Block] { 

          case "heading1"  => __.read(Heading.reader(1)).map(identity[Block])
          case "heading2"  => __.read(Heading.reader(2)).map(identity[Block])
          case "heading3"  => __.read(Heading.reader(3)).map(identity[Block])
          case "heading4"  => __.read(Heading.reader(4)).map(identity[Block])
          case "paragraph" => __.read[Paragraph].map(identity[Block])

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
  fragments: Map[String, Fragment]
) {
  def apply(field: String): Fragment = fragments(field)

  def get(field: String): Option[Fragment] = fragments.get(field)

  def getImage(field: String): Option[Fragment.Image] = get(field).flatMap {
    case a: Fragment.Image => Some(a)
    case _ => None
  }

  def getStructuredText(field: String): Option[Fragment.StructuredText] = get(field).flatMap {
    case a: Fragment.StructuredText => Some(a)
    case _ => None
  }

  def getText(field: String): Option[String] = get(field).flatMap {
    case a: Fragment.StructuredText => Some(a.blocks.collect { case b: Fragment.StructuredText.Block.TextBlock => b.text }.mkString("\n")).filterNot(_.isEmpty)
    case _ => None
  }

}

object Document {
  implicit val reader = (
    (__ \ "id").read[String] and
    (__ \ "type").read[String] and
    (__ \ "href").read[String] and
    (__ \ "tags").read[Seq[String]] and
    (__ \ "data").read[JsObject].map { data =>
      data.fields.map { 
        case (key, jsvalue) => 
          (jsvalue \ "type").asOpt[String].flatMap {

            case "Image" => Some(Fragment.Image.reader.map(identity[Fragment]))
            case "StructuredText" => Some(Fragment.StructuredText.reader.map(identity[Fragment]))

            case t => None
          }.flatMap(_.reads(jsvalue \ "data").asOpt).toList.map(fragment => (key, fragment))
      }.flatten.toMap
    }
  )( Document.apply _ )

}
