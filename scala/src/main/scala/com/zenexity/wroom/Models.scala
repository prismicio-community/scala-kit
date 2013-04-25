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

sealed trait Field {
  def typ: String
  def multiple: Boolean = false
}

case class SingleField(override val typ: String, default: Option[String]) extends Field {
  override val multiple: Boolean = false
}
object SingleField {
  implicit val reader = (
    (__ \ "type").read[String] and
    //((__ \ "multiple").read[Boolean] orElse Reads.pure(false)) and
    (__ \ "default").readNullable[String]
  )(SingleField.apply _)

  val writer = (
    (__ \ "type").write[String] and
    (__ \ "multiple").write[Boolean] and
    (__ \ "default").writeNullable[String]
  )((field: SingleField) => (field.typ, field.multiple, field.default))

}
case class MultipleField(override val typ: String, default: Seq[String]) extends Field {
  override val multiple: Boolean = true
}
object MultipleField {
  implicit val reader = (
    (__ \ "type").read[String] and
    //((__ \ "multiple").read[Boolean] orElse Reads.pure(false)) and
    ((__ \ "default").read[Seq[String]] orElse Reads.pure(Seq()))
  )(MultipleField.apply _)

  val writer = (
    (__ \ "type").write[String] and
    (__ \ "multiple").write[Boolean] and
    (__ \ "default").write[Seq[String]]
  )((field: MultipleField) => (field.typ, field.multiple, field.default))
}

object Field {
  implicit val reader = 
    (
      (__ \ 'multiple).read[Boolean] and
      __.json.pick
    ).tupled flatMap { case (multiple, js) => multiple match {
      case true   => Reads{ _ => Json.fromJson[MultipleField](js) } map { c => c:Field }
      case false  => Reads{ _ => Json.fromJson[SingleField](js) } map { c => c:Field }
    } }

  implicit val writer = Writes[Field]{ field => field match {
    case s: SingleField   => Json.toJson[SingleField](s)(SingleField.writer)
    case s: MultipleField => Json.toJson[MultipleField](s)(MultipleField.writer)
  }}
}

case class Form(
  name: Option[String], 
  method: String, 
  rel: Option[String], 
  enctype: String, 
  action: String,
  fields: Map[String, Field]
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
    (__ \ 'aliases).read[Map[String, String]] and
    (__ \ 'forms).read[Map[String, Form]]
  )(ApiData.apply _)

  implicit val writer = Json.writes[ApiData]
}


case class Doc(
  id: String, 
  tags: Seq[String], 
  data: JsValue
)
object Doc {
  implicit val reader = Json.reads[Doc]
}
