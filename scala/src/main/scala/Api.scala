package io.prismic

import org.joda.time._

import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import core._

case class Api(data: ApiData, cache: Cache, logger: (String) => Unit) {

  def refs: Map[String, Ref] = data.refs.groupBy(_.label).mapValues(_.head)
  def bookmarks: Map[String, String] = data.bookmarks
  def forms: Map[String, SearchForm] = data.forms.mapValues(form => SearchForm(this, form, form.defaultData))

  def master: Ref = refs.values.collectFirst { case ref if ref.isMasterRef => ref }.getOrElse(sys.error("no master reference found"))

}

object Api {

  val AcceptJson = Map("Accept" -> Seq("application/json"))
  val MaxAge = """max-age\s*=\s*(\d+)""".r

  def get(url: String, cache: Cache = NoCache, logger: (String) => Unit = identity): Future[Api] = {
    CustomWS.url(url)
      .copy(headers = AcceptJson)
      .get()
      .map { resp =>
        resp.status match {
          case 200    => Api(ApiData.reader.reads(resp.json).get, cache, logger)
          case error  => sys.error(s"Http error $error (${resp.statusText}")
        }
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

case class SearchForm(api: Api, form: Form, data: Map[String,String]) {

  def ref(r: Ref): SearchForm = ref(r.ref)
  def ref(r: String): SearchForm = copy(data = data ++ Map("ref" -> r))

  def query(query: String) = {
    def strip(q: String) = q.trim.drop(1).dropRight(1)
    copy(data = data ++ Map("q" -> (s"[${form.fields("q").default.map(strip).getOrElse("")}${strip(query)}]")))
  }

  def submit(): Future[Seq[Document]] = {
    implicit val documentReader: Reads[Document] = Document.reader(api.data)

    def parseResult(json: JsValue) = Json.fromJson[Seq[Document]](json).recoverTotal { e => sys.error(s"unable to parse Document: $e") }

    (form.method, form.enctype, form.action) match {
      case ("GET", "application/x-www-form-urlencoded", action) =>

        val url = {
          val encoder = new org.jboss.netty.handler.codec.http.QueryStringEncoder(form.action)
          data.foreach {
            case (key, value) => encoder.addParam(key, value)
          }
          encoder.toString()
        }

        api.cache.get(url).map { json =>
          Future.successful(parseResult(json))
        }.getOrElse {
          CustomWS.url(url).copy(headers = Api.AcceptJson).get() map { resp =>
            resp.status match {
              case 200 => 
                val json = resp.json

                resp.header("Cache-Control").foreach {
                  case Api.MaxAge(duration) => api.cache.set(url, (System.currentTimeMillis + duration.toLong * 1000, json))
                  case _ => 
                }

                parseResult(json)
              case error => sys.error(s"Http error(status:$error msg:${resp.statusText}")
            }
          }
        }

      case _ =>
        sys.error(s"Form type not supported")
    }
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

  def isTagged(requiredTags: Seq[String]) = requiredTags.forall(tag => tags.contains(tag))

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

  def getHtml(field: String, linkResolver: LinkResolver): Option[String] = {
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

  implicit def reader(apiData: ApiData) = (
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
              case "StructuredText" => Some(Fragment.StructuredText.reader(apiData).map(identity[Fragment]))

              case t => None
            }.flatMap(_.reads(jsvalue \ "value").asOpt).toList.map(fragment => (s"$typ.$key", fragment))
        }.flatten.toMap
      }.map(data => (typ,data))
    }
  )((id, href, tags, slugs, typAndData) => Document(id, typAndData._1, href, tags, slugs, typAndData._2))

}



