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
  def document(id: String) = form("document").withFields("id" -> id)

  override def toString = s"""Api:
    -> data: ${Json.prettyPrint(Json.toJson(data))}
  """
}

case class FormApi(
  api: Api,
  form: Form,
  fieldFilters: Seq[FieldFilter] = Seq(),
  chunkFilters: Seq[ChunkFilter] = Seq()
){
  def withRef(ref: Ref): RefFormApi = RefFormApi(this, ref)

  def withFields(theFieldFilters: FieldFilter*): FormApi = copy( fieldFilters = fieldFilters ++ theFieldFilters )
  def withFields(theFieldFilter: (String, Any), theFieldFilters: (String, Any)*): FormApi = withFields((theFieldFilter +: theFieldFilters).map{ case(k,v) => FieldFilter(k,v) }:_*)

  def withChunks(theChunkFilters: ChunkFilter*): FormApi = copy( chunkFilters = chunkFilters ++ theChunkFilters )
  def withChunks(theChunkFilter: (String, Render), theChunkFilters: (String, Render)*): FormApi = withChunks((theChunkFilter +: theChunkFilters).map{ case (name, render) => ChunkFilter(name, render) }: _*)
  def withChunks(theChunkFilter: String, theChunkFilters: String*): FormApi = withChunks((theChunkFilter +: theChunkFilters).map{ name => ChunkFilter(name) }: _*)
}

case class RefFormApi(
  formapi: FormApi,
  ref: Ref
) {

  def withRef(ref: Ref): RefFormApi = copy(ref = ref)

  def withFields(theFieldFilters: FieldFilter*): RefFormApi = copy( formapi = formapi.withFields(theFieldFilters:_*) )
  def withFields(theFieldFilter: (String, Any), theFieldFilters: (String, Any)*): RefFormApi = copy( formapi = formapi.withFields(theFieldFilter, theFieldFilters:_*) )

  def withChunks(theChunkFilters: ChunkFilter*): RefFormApi = copy( formapi = formapi.withChunks(theChunkFilters:_*) )
  def withChunks(theChunkFilter: (String, Render), theChunkFilters: (String, Render)*): RefFormApi = copy( formapi = formapi.withChunks(theChunkFilter, theChunkFilters:_*) )
  def withChunks(theChunkFilter: String, theChunkFilters: String*): RefFormApi = copy( formapi = formapi.withChunks(theChunkFilter, theChunkFilters:_*) )

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
    val m = Map("ref" -> Seq(java.net.URLEncoder.encode(ref.ref))) ++
    formapi.fieldFilters.map{ case FieldFilter(k,v) =>
      // converting field according to form fields
      formapi.form.fields.get(k) match {
        case Some(field) => k -> urlEncodeField(field.typ, field.multiple, v)
        case None        => sys.error(s"Form doesn't accept the field $k")
      }
    }

    (if(!formapi.chunkFilters.isEmpty) m + ("widgets" -> formapi.chunkFilters.map{ w => w.toString })
    else m) ++
    formapi.api.queryString

    // TODO add default values

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
              println("REC:"+js)
              Json.fromJson[Seq[Document]](js).recoverTotal{ e => sys.error("unable to parse Document: "+e) }
            case error => throw new java.lang.RuntimeException(s"Http error(status:$error msg:${resp.statusText}")
          }
        }
      case _ => sys.error("only GET is managed right now")
    }
  }

}

case class FieldFilter(name: String, value: Any)

case class ChunkFilter(name: String, render: Render = Render.HTML, props: Map[String, String] = Map()) {
  def withProps(theProps: (String, String)*) = copy(props = props ++ theProps)

  def toMap: Map[String, String] = Map("name" -> name, "render" -> render.toString) ++ props
  override def toString = toMap.map{ case(k,v) => s"$k:$v"}.mkString("(", ",", ")")
}

