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
  val forms: Map[String, Form]
)
object ApiData {
  //implicit val reader = Json.reads[ApiData]
  implicit val reader = (
    (__ \ 'refs).read[Seq[Ref]] and
    (__ \ 'bookmarks).read[Map[String, String]] and
    (__ \ 'forms).read[Map[String, Form]]
  )(ApiData.apply _)

  implicit val writer = Json.writes[ApiData]
}


sealed trait Render
object Render {
  case object HTML extends Render {
    override def toString = "html"
  }
  case object JSON extends Render {
    override def toString = "json"
  }
}

sealed trait Chunk {
  def name: String
  def render: Render

  def asHtml: String = this match {
    case c: HtmlChunk => c.content
    case _ => throw new RuntimeException("can't convert chunk to HtmlChunk")
  }

  def asImage: JsonChunk.Image = this match {
    case c: JsonChunk.Image => c
    case _ => throw new RuntimeException("can't convert chunk to Image")
  }

  def asText: JsonChunk.StructuredText = this match {
    case c: JsonChunk.StructuredText => c
    case _ => throw new RuntimeException("can't convert chunk to StructuredText")
  }
}

case class HtmlChunk(name: String, content: String) extends Chunk {
  override val render = Render.HTML

  override def toString = s"""HtmlChunk.$render($name, $content)"""
}

object HtmlChunk{
  val writer = Writes[HtmlChunk]{ c => Json.obj(c.name -> c.content) }
}

sealed trait JsonChunk extends Chunk {
  override val render = Render.JSON
}

object JsonChunk{
  case class StructuredText(name: String, elements: Seq[JsObject]) extends JsonChunk{
    override def toString = s"""JsonChunk.StructuredText($name,${JsArray(elements)})"""
  }
  case class Image(name: String, elements: JsObject) extends JsonChunk{
    override def toString = s"""JsonChunk.Image($name,$elements)"""
  }

  object StructuredText{
    implicit val reader = Json.reads[StructuredText]
    val writer = Json.writes[StructuredText]
  }

  object Image{
    implicit val reader = Json.reads[Image]
    val writer = Json.writes[Image]
  }

  def reader(name: String): Reads[JsonChunk] = (
    (__ \ "type").read[String] and
    (__ \ "data").read[JsValue]
  ).tupled.map{
    case ("StructuredText", arr) => StructuredText(name, arr.as[Seq[JsObject]])
    case ("Image", obj) => Image(name, obj.as[JsObject])
    case _ => sys.error("unmanaged Json Chunk type")
  }

  // not implicit to prevent problems with contravariance
  val writer = Writes[JsonChunk] {
    case st:StructuredText => Json.toJson(st)(StructuredText.writer)
    case st:Image => Json.toJson(st)(Image.writer)
  }

}

object Chunk {
  //implicit val reader: Reads[Chunk] = __.read[HtmlChunk].map(c=>c:Chunk) orElse __.read[JsonChunk].map(c=>c:Chunk)

  implicit val writer = Writes[Chunk]{
    case a:HtmlChunk => Json.toJson(a)(HtmlChunk.writer)
    case a:JsonChunk => Json.toJson(a)(JsonChunk.writer)
  }
}

case class Document(
  id: String, typ: String, href: String, 
  tags: Seq[String], chunks: Map[String, Chunk]
) {
  def apply(field: String): Chunk = chunks(field)
  def get(field: String): Option[Chunk] = chunks.get(field)
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
          case JsString(s)  => (k -> (HtmlChunk(name=k, content=s): Chunk) )
          case obj: JsObject => (k -> (obj.as[JsonChunk](JsonChunk.reader(k)): Chunk) )
          /*(
            (__ \ "type").read[String] and
            (__ \ "data").read[JsValue]
          ).tupled.map{
            case (typ, JsString(s)) => (k -> Chunk.HtmlChunk(name=k, typ=typ, content=s))
            case (typ, data: JsValue) => (k -> Chunk.Json)
          }
          case arr: JsArray =>
            arr.validate[Seq[JsObject]]
               .map{ elts =>
                 (k -> Widget.JSON(name=k, elements=elts))
               }.recoverTotal { e => sys.error("unable to parse Document: "+e) }*/
          case _ => sys.error("unmanaged document format")
        }
      }.toMap
    }
  )( Document.apply _ )

  implicit val writer = Json.writes[Document]
}
