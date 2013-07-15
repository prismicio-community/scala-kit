package com.zenexity.wroom.client

import scala.concurrent._
import org.joda.time._

import play.api.libs.json._
import play.api.libs.functional.syntax._

trait Api {
  def data: ApiData
  def baseUrl: String
  def queryString: Map[String, Seq[String]]
  def headers: Map[String, Seq[String]]

  def refs: Map[String, Ref] = data.refs.groupBy(_.label).mapValues(_.head)
  def bookmarks: Map[String, String] = data.bookmarks
  def forms: Map[String, Form] = data.forms

  def ref(refId: String): Ref = refs(refId)
  def getRef(refId: String): Option[Ref] = refs.get(refId)
  def master: Ref = data.refs.collectFirst{ case ref if ref.isMasterRef => ref } match {
    case Some(r) => r
    case None    => sys.error("potential problem: no master reference found")
  }

  def bookmark(name: String): String = bookmarks(name)

  def form(s: String): FormApi = FormApi(this, forms(s))
  def getForm(s: String): Option[FormApi] = forms.get(s) map { f => FormApi(this, f) }

  def documents = form("documents")
  def document = form("document")

  override def toString = s"""Api:
-> apidata: ${Json.prettyPrint(Json.toJson(data))}
  """
}

case class FormApi(
  api: Api,
  form: Form,
  q: Option[Query] = None
) {
  def ref(r: Ref): RefFormApi = RefFormApi(this, r)
  def q(query: String) = copy(q = Some(StringQuery(query)))
}

case class RefFormApi(
  formapi: FormApi,
  ref: Ref
) {

  private def urlEncodeField(typ: String, multiple: Boolean, value: Any): Seq[String] = {
    typ match {
      case "String" => multiple match {
        case false  => Seq(java.net.URLEncoder.encode(value.toString))
        case true => value.asInstanceOf[Seq[Any]].map(e => java.net.URLEncoder.encode(e.toString))
      }
      case _ => sys.error("unknown type")
    }
  }

  lazy val queryString: Map[String, Seq[String]] = {
    Map(
      "ref" -> Seq(java.net.URLEncoder.encode(ref.ref))
    ) ++ 
    (
      if(formapi.q.isDefined) Map("q" -> Seq(formapi.q.get.toQueryString))
      else Map()
    ) ++
    formapi.api.queryString
  }

  lazy val headers = formapi.api.headers

  def submit()(implicit ctx: ExecutionContext): Future[Seq[Document]] = {
    val method = formapi.form.method
    val enctype = formapi.form.enctype
    var holder = CustomWS.url(formapi.form.action)
    method match {
      case "GET" =>
        enctype match {
          case "application/x-www-form-urlencoded" =>
            holder = holder.copy(queryString = queryString)
          case _ => sys.error("enctype not managed")
        }
        // JSON for now
        holder = holder.copy(headers = headers)
        holder.get() map { resp =>
          resp.status match {
            case 200 =>
              val js = resp.json
              Json.fromJson[Seq[Document]](js).recoverTotal{ e => sys.error("unable to parse Document: "+e) }
            case error => throw new java.lang.RuntimeException(s"Http error(status:$error msg:${resp.statusText}")
          }
        }
      case _ => sys.error("only GET is managed right now")
    }
  }

}


sealed trait Query {
  def toQueryString: String
}
case class StringQuery(q: String) extends Query {
  def toQueryString = q
}


