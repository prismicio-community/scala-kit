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
  def name: String

  def asHtml: String

  def asStructuredText: Option[Fragment.StructuredText] = this match {
    case a: Fragment.StructuredText => Some(a)
    case _ => None
  }
}


object Fragment{

  /**
    * StructuredText
    */
  case class StructuredText(name: String, blocks: Seq[StructuredText.Block]) extends Fragment {
    import StructuredText._
    def asHtml: String = blocks.map {
      case Block.Text(label, content, meta) =>
        val t = asMeaningText(meta, content)
        val html = label match {
          case "heading1" => s"""<h1>$t</h1>"""
          case "heading2" => s"""<h2>$t</h2>"""
          case "heading3" => s"""<h3>$t</h3>"""
          case "paragraph" => s"""<p>$t</p>"""
          case "preformatted" => s"""<pre>$t</pre>"""
          case "list-item" => s"""<li>$t</li>"""
          case _ => t
        }
        html

      case Block.Image(url, width, height) =>
        s"""<img src="${url}" width="${width}" height="${height}">"""
    }.mkString

    import scala.collection.mutable.ListBuffer

    private def escape(text: String, preserveWhitespace: Boolean): String = {
      text.replace("<", "&lt;")
          .replace("\n", "<br>")
          .replace(" ", if(preserveWhitespace) "&nbsp;" else " ")
    }

    private def escape(char: Char, preserveWhitespace: Boolean): String =
      escape(char.toString, preserveWhitespace)

    def linkAsHtml(meta: StructuredText.Meta): Option[String] = {
      /*import StructuredText.links._

      meta.asMediaLink.map {
        case ImageLink(id, url, date, height, width, length, name, kind, _) =>
          s"""<a href="$url" data-link-to="media" data-media-id="$id" data-filename="$name" data-type="$kind" data-width="$width" data-height="$height" data-length="$length">"""
        case FileLink(id, url, name, kind, date, length, _) =>
          s"""<a href="$url" data-link-to="media" data-media-id="$id" data-filename="$name" data-type="$kind" data-length="$length">"""
        case x => Wroom.oops("Can't handle this media link: " + x)
      } orElse {
        meta.asDocumentLink.map {
          case DocumentLink(id, typ, _) =>
            s"""<a data-link-to="document" data-document-id="$id" data-type="$typ">"""
        }
      } orElse {
        meta.asExternalLink.map { url =>
          s"""<a href="$url" data-link-to="web">"""
        }
      }*/

      None
    }
    def asMeaningText(
      meta: Seq[StructuredText.Meta],
      text: String,
      preserveWhitespace: Boolean = false
    ): String = {

      def asHtmlTag(typ: String, meta: Option[StructuredText.Meta], opening: Boolean): String = {
        typ match {
          case "em" => if(opening) "<em>" else "</em>"
          case "strong" => if(opening) "<strong>" else "</strong>"
          case "hyperlink" => if(opening) {
            meta.flatMap(linkAsHtml).getOrElse { println(meta); "<a>" }
          } else """</a>"""
          case typ => if(opening) s"""<span class="$typ">""" else "</span>"
        }
      }

      def asHtml(endingsToApply: Seq[(Int, String)], startingsToApply: Seq[StructuredText.Meta]): String = {
        endingsToApply.map(e => asHtmlTag(e._2, None, opening = false)).mkString("") +
        startingsToApply.map(s => asHtmlTag(s.`type`, Some(s), opening = true)).mkString("")
      }

      @scala.annotation.tailrec
      def step(in: Seq[(Char, Int)], startings: Seq[StructuredText.Meta], endings: Seq[(Int, String)] = Nil, html: ListBuffer[String] = ListBuffer()): String = {
        val nextOp = (startings.headOption.map(_.start).toList ++ endings.headOption.toList.map(_._1)).reduceOption(Math.min)
        in match {
          case ((_, pos) :: tail) if !nextOp.exists( _ == pos) => {
            val (done,toDo) = in.toList.span(i => ! nextOp.exists(i._2 == _))
            step(toDo, startings, endings, html += escape(done.map(_._1).mkString, preserveWhitespace))
          }
          case (current, pos) :: tail => {
            val (endingsToApply, othersEnding) = endings.span { case (end, _) => end == pos }
            val (startingsToApply, othersStarting) = startings.span(_.start == pos)
            val caractere = asHtml(endingsToApply, startingsToApply) + escape(current, preserveWhitespace)
            val moreEndings = startingsToApply.reverse.map(s => s.end -> s.`type`)
            val newEndings = moreEndings ++ othersEnding
            step(tail, othersStarting, newEndings, html += caractere)
          }
          case Nil => html.mkString("") + asHtml(endings, Seq.empty).mkString("")
        }
      }
      step(text.toList.zipWithIndex, meta.sortBy(_.start))
    }
  }

  object StructuredText{
    sealed trait Meta {
      def start: Int
      def end: Int
      def `type`: String
    }

    object Meta{

      case class Em(start: Int, end: Int) extends Meta{
        override val `type` = "em"
      }
      object Em {
        val writer = Json.writes[Em]
      }

      case class Strong(start: Int, end: Int) extends Meta{
        override val `type` = "strong"
      }
      object Strong {
        val writer = Json.writes[Strong]
      }

      trait HyperLink extends Meta{
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

        case class EmptyLink(start: Int, end: Int) extends HyperLink{
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

      implicit val reader: Reads[Meta] =
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
      implicit val writer = Writes[Meta]{
        case a:Em => Json.toJson(a)(Em.writer)
        case a:Strong => Json.toJson(a)(Strong.writer)
        case a:HyperLink => Json.toJson(a)(HyperLink.writer)
      }
    }

    /**
      * StructuredText Blocks
      */
    sealed trait Block

    object Block {
      /**
        * Text Block
        */
      case class Text(label: String, content: String, meta: Seq[Meta]) extends Block
      object Text {
        implicit val reader = Json.reads[Text]
        val writer=  Json.writes[Text]
      }

      /**
        * Image Block
        */
      case class Image(url: String, width: Int, height: Int) extends Block
      object Image {
        implicit val reader = (
          (__ \ 'url).read[String] and
          (__ \ 'dimensions).read((
            (__ \ 'width).read[Int] and
            (__ \ 'height).read[Int]
          ).tupled)
        ){ (url, dim) => Image(url, dim._1, dim._2) }

        val writer = (
          (__ \ 'url).write[String] and
          (__ \ 'dimensions \ 'width).write[Int] and
          (__ \ 'dimensions \ 'height).write[Int]
        )(unlift(Image.unapply))
      }

      /**
        * Block Reads/Writes
        */
      implicit val reader =
        __.read[Text](Text.reader).map{a => a:Block} orElse
        __.read[Image](Image.reader).map{a => a:Block}

      implicit val writer = Writes[Block]{
        case t: Text => Json.toJson(t)(Text.writer)
        case i: Image => Json.toJson(i)(Image.writer)
      }
    }

    /**
      * StructuredText Reads/Writes
      */
    implicit val reader = Json.reads[StructuredText]
    val writer = Json.writes[StructuredText]

  }
/*
  /**
    * Image
    */
  case class Image(view: Image.View, thumbnails: Map[String, Image.View]) extends Fragment

  object Image{
    case class View(origin: (String, Int, Int), url: Option[String], width: Int, height: Int, zoom: Float, crop: (Int,Int), background: String)
  }

  /**
    * Maps
    */
  case class Maps(address:String, lat:Double, lng:Double) extends Fragment

  /**
    * Embed
    */
  case class Embed(
    url: String, `type`: String, provider: Option[String], title: Option[String], preview: Option[(String, Int, Int)]
  ) extends Fragment


  /**
    * Field
    */
  case class Field(value: String, typ: String) extends Fragment
*/

  def reader(name: String): Reads[Fragment] = (
    (__ \ "type").read[String] and
    (__ \ "data").read[JsValue]
  ).tupled.map{
    case ("StructuredText", d) => StructuredText(name, d.as[Seq[StructuredText.Block]])
    case (t, _) => sys.error(s"unmanaged Json Fragment type $t")
  }

  // not implicit to prevent problems with contravariance
  implicit val writer = Writes[Fragment] {
    case st: StructuredText => Json.toJson(st)(StructuredText.writer)
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
}

object Document {
  implicit val reader = (
    (__ \ "id").read[String] and
    (__ \ "type").read[String] and
    (__ \ "href").read[String] and
    (__ \ "tags").read[Seq[String]] and
    (__ \ "data").read[JsObject].map{ obj =>
      obj.fields.map{ case (k, v) =>
        v match {
          case obj: JsObject => (k -> (obj.as[Fragment](Fragment.reader(k)): Fragment) )
          case _ => sys.error("unmanaged document format")
        }
      }.toMap
    }
  )( Document.apply _ )

  implicit val writer = Json.writes[Document]
}
