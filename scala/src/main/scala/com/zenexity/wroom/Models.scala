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

sealed trait Widget {
  def name: String
  def render: Render
}
object Widget {
  case class Html(name: String, content: String) extends Widget {
    override val render = Render.HTML
  }
  case class Json(name: String, elements: Seq[JsObject]) extends Widget {
    override val render = Render.JSON
  }
  
}

case class Document(ref: String, tags: Seq[String], widgets: Map[String, Widget])

