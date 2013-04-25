package com.zenexity.wroom.client

import scala.concurrent._
import org.joda.time._

import play.api.libs.json._
import play.api.libs.functional.syntax._

trait Api {
  def data: ApiData
  def baseUrl: String

  def refs: Map[String, Ref] = data.refs.groupBy(_.label).mapValues(_.head)
  def bookmarks: Map[String, String] = data.bookmarks
  def forms: Map[String, Form] = data.forms

  def master: RefApi = data.refs.collectFirst{ case ref if ref.isMasterRef => ref } match {
    case Some(r) => RefApi(this, r)
    case None    => sys.error("potential problem: no master reference found")
  }

  def ref(refId: String): RefApi = RefApi(this, refs(refId))
  def getRef(refId: String): Option[RefApi] = refs.get(refId) map { r => RefApi(this, r) }

  def bookmark(name: String): String = bookmarks(name)

  override def toString = s"""Api:
    -> data: ${Json.prettyPrint(Json.toJson(data))}
  """
}

case class RefApi(api: Api, ref: Ref) {
  def form(s: String): FormApi = FormApi(this, api.forms(s))
  def getForm(s: String): Option[FormApi] = api.forms.get(s) map { f => FormApi(this, f) }

  def documents = form("documents")
  def document(id: String) = form("document").withTags("id" -> id).submit()
}

case class FormApi(refapi: RefApi, form: Form, tags: Map[String, Any] = Map()) {
  def withTags(t: (String, Any)*): FormApi = this.copy( tags = tags ++ t )

  private def urlEncodeField(typ: String, multiple: Boolean, value: Any): Seq[String] = {
    typ match {
      case "String" => multiple match {
        case false  => Seq(java.net.URLEncoder.encode(value.toString))
        case true => value.asInstanceOf[Seq[Any]].map(e => java.net.URLEncoder.encode(e.toString))
      }
      case _ => sys.error("unknown type")
    }
  }

  private def queryString: Map[String, Seq[String]] = {
    (Map("ref" -> Seq(java.net.URLEncoder.encode(refapi.ref.ref))) ++ 
    tags.map{ case(k,v) => 
      // converting field according to form fields
      form.fields.get(k) match {
        case Some(field) => k -> urlEncodeField(field.typ, field.multiple, v)
        case None        => sys.error("Form doesn't accept this field")
      }
      
    })

    // TODO add default values
  }
  
  def submit(): Future[Response] = {
    val method = form.method
    val enctype = form.enctype
    var holder = WS.url(refapi.api.baseUrl + form.action)
    method match {
      case "GET" => 
        enctype match {
          case "application/x-www-form-urlencoded" => 
            holder = holder.copy(queryString = queryString)
          case _ => sys.error("enctype not managed")
        }
        // JSON for now
        holder = holder.withHeaders("Accept" -> "application/json")
        println("holder:"+holder)
        holder.get()
    }
  }

}