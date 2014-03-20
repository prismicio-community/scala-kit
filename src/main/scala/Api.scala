package io.prismic

import org.joda.time._

import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

import core._

sealed trait ApiError extends RuntimeException {
  def message: String
  override def getMessage = message
}

case class AuthorizationNeeded(message: String, oAuthUrl: String) extends ApiError
case class InvalidToken(message: String, oAuthUrl: String) extends ApiError
case class UnexpectedError(message: String) extends ApiError

case class Api(data: ApiData, accessToken: Option[String], cache: Cache, logger: (Symbol, String) => Unit) {
  def refs: Map[String, Ref] = data.refs.groupBy(_.label).mapValues(_.head)
  def bookmarks: Map[String, String] = data.bookmarks
  def forms: Map[String, SearchForm] = data.forms.mapValues(form => SearchForm(this, form, form.defaultData))
  def master: Ref = refs.values.collectFirst { case ref if ref.isMasterRef => ref }.getOrElse(sys.error("no master reference found"))

  def oauthInitiateEndpoint = data.oauthEndpoints._1
  def oauthTokenEndpoint = data.oauthEndpoints._2
}

trait DocumentLinkResolver {
  def apply(link: Fragment.DocumentLink): String
  def apply(document: Document): String = apply(
    Fragment.DocumentLink(document.id, document.typ, document.tags, document.slug, false)
  )
}

object DocumentLinkResolver {

  def apply(api: Api)(f: (((Fragment.DocumentLink, Option[String])) => String)) = new DocumentLinkResolver {
    def apply(link: Fragment.DocumentLink): String = f((link, api.bookmarks.find(_._2 == link.id).map(_._1)))
  }

  def apply(f: Fragment.DocumentLink => String) = new DocumentLinkResolver {
    def apply(link: Fragment.DocumentLink): String = f(link)
  }

}

object Api {

  val AcceptJson = Map("Accept" -> Seq("application/json"))
  val MaxAge = """max-age\s*=\s*(\d+)""".r

  def get(url: String, accessToken: Option[String] = None, cache: Cache = NoCache, logger: (Symbol, String) => Unit = { (_, _) => () }): Future[Api] = {
    CustomWS.url(logger, accessToken.map(token => s"$url?access_token=$token").getOrElse(url))
      .copy(headers = AcceptJson)
      .get()
      .map { resp =>
        resp.status match {
          case 200                          => Api(ApiData.reader.reads(resp.json).getOrElse(sys.error(s"Error while parsing API document: ${resp.json}")), accessToken, cache, logger)
          case 401 => (resp.json \ "oauth_initiate").asOpt[String] match {
            case Some(url) if accessToken.isDefined =>
              throw InvalidToken("The provided access token is either invalid or expired", url)
            case Some(url) =>
              throw AuthorizationNeeded("You need to provide an access token to access this repository", url)
            case None =>
              throw UnexpectedError("Authorization error, but not URL was provided")
          }
          case err => throw UnexpectedError(s"Got an HTTP error $err (${resp.statusText})")
        }
      }
  }

}

case class Ref(
  ref: String,
  label: String,
  isMasterRef: Boolean = false,
  scheduledAt: Option[DateTime] = None)

object Ref {

  implicit val reader = (
    (__ \ "ref").read[String] and
    (__ \ "label").read[String] and
    ((__ \ "isMasterRef").read[Boolean] orElse Reads.pure(false)) and
    (__ \ "scheduledAt").readNullable[DateTime]
  )(Ref.apply _)

}

case class Field(`type`: String, multiple: Boolean, default: Option[String])

object Field {
  implicit val reader = (
    (__ \ "type").read[String] and
    (__ \ "multiple").readNullable[Boolean].map(_.getOrElse(false)) and
    (__ \ "default").readNullable[String]
  )(Field.apply _)
}

case class Form(
    name: Option[String],
    method: String,
    rel: Option[String],
    enctype: String,
    action: String,
    fields: Map[String, Field]) {

  def defaultData: Map[String, Seq[String]] = {
    fields.mapValues(_.default).collect {
      case (key, Some(value)) => (key, Seq(value))
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
  val forms: Map[String, Form],
  val oauthEndpoints: (String, String))

object ApiData {

  implicit val reader = (
    (__ \ 'refs).read[Seq[Ref]] and
    (__ \ 'bookmarks).read[Map[String, String]] and
    (__ \ 'types).read[Map[String, String]] and
    (__ \ 'tags).read[Seq[String]] and
    (__ \ 'forms).read[Map[String, Form]] and
    (
      (__ \ 'oauth_initiate).read[String] and
        (__ \ 'oauth_token).read[String] tupled
    )
  )(ApiData.apply _)

}

case class Response(
  results: List[Document],
  page: Int,
  resultsPerPage: Int,
  resultsSize: Int,
  totalResultsSize: Int,
  totalPages: Int,
  nextPage: Option[String],
  prevPage: Option[String])

object Response {

    private implicit val documentReader: Reads[Document] = Document.reader
    val jsonReader = (
      (__ \ "results").read[List[Document]] and
      (__ \ "page").read[Int] and
      (__ \ "results_per_page").read[Int] and
      (__ \ "results_size").read[Int] and
      (__ \ "total_results_size").read[Int] and
      (__ \ "total_pages").read[Int] and
      (__ \ "next_page").readNullable[String] and
      (__ \ "prev_page").readNullable[String]
    )(Response.apply _)
}

case class SearchForm(api: Api, form: Form, data: Map[String, Seq[String]]) {

  def set(field: String, value: String): SearchForm = form.fields.get(field).map { fieldDesc =>
    copy(data = data ++ Map(field -> (if (fieldDesc.multiple) data.get(field).getOrElse(Nil) ++ Seq(value) else Seq(value))))
  }.getOrElse(sys.error(s"Unknown field $field"))

  def set(field: String, value: Int): SearchForm = form.fields.get(field).map(_.`type`).map {
    case "Integer" => set(field, value.toString)
    case t         => sys.error(s"Cannot use a Int as value for the field $field of type $t")
  }.getOrElse(sys.error(s"Unknown field $field"))

  def ref(r: Ref): SearchForm = ref(r.ref)
  def ref(r: String): SearchForm = set("ref", r)

  def query(query: String) = {
    if (form.fields.get("q").map(_.multiple).getOrElse(false)) {
      set("q", query)
    }
    else {
      // Temporary Hack for backward compatibility
      def strip(q: String) = q.trim.drop(1).dropRight(1)
      copy(data = data ++ Map("q" -> Seq((s"""[${form.fields("q").default.map(strip).getOrElse("")}${strip(query)}]"""))))
    }
  }

  def page(p: Int) = set("page", p)
  def pageSize(p: Int) = set("pageSize", p)

  def orderings(o: String) = set("orderings", o)

  def submit(): Future[Response] = {

    def parseResponse(json: JsValue): Response = Response.jsonReader reads json match {
      case JsSuccess(result, _) => result
      case JsError(err)         => sys.error(s"Unable to parse prismic response: $json\n$err")
    }

    (form.method, form.enctype, form.action) match {
      case ("GET", "application/x-www-form-urlencoded", action) =>

        val url = {
          val encoder = new org.jboss.netty.handler.codec.http.QueryStringEncoder(form.action)
          data.foreach {
            case (key, values) => values.foreach(value => encoder.addParam(key, value))
          }
          encoder.toString()
        }

        api.cache.get(url).map { json =>
          Future.successful(parseResponse(json))
        }.getOrElse {
          CustomWS.url(api.logger, url).copy(headers = Api.AcceptJson).get() map { resp =>
            resp.status match {
              case 200 =>
                val json = resp.json

                resp.header("Cache-Control").foreach {
                  case Api.MaxAge(duration) => api.cache.set(url, (System.currentTimeMillis + duration.toLong * 1000, json))
                  case _                    =>
                }

                parseResponse(json)
              case error => sys.error(s"Http error(status:$error msg:${resp.statusText}")
            }
          }
        }

      case _ => Future.failed {
        sys.error(s"Form type not supported")
      }
    }
  }

}

trait WithFragments {

  def fragments: Map[String, Fragment]

  private val IndexedKey = """^([^\[]+)(\[\d+\])?$""".r

  def apply(field: String): Fragment = fragments(field)

  def get(field: String): Option[Fragment] = fragments.get(field).orElse(getAll(field).headOption)

  def getAll(field: String): Seq[Fragment] = fragments.collect {
    case (IndexedKey(key, _), fragment) if key == field => fragment
  }.toSeq

  def getLink(field: String): Option[Fragment.Link] = get(field).flatMap {
    case a: Fragment.WebLink      => Some(a)
    case a: Fragment.MediaLink    => Some(a)
    case a: Fragment.DocumentLink => Some(a)
    case _                        => None
  }

  def getImage(field: String): Option[Fragment.Image] = get(field).flatMap {
    case a: Fragment.Image          => Some(a)
    case a: Fragment.StructuredText => a.blocks.collectFirst { case b: Fragment.StructuredText.Block.Image => b.view }.map(v => Fragment.Image(v))
    case _                          => None
  }

  def getAllImages(field: String): Seq[Fragment.Image] = getAll(field).flatMap {
    case a: Fragment.Image          => Seq(a)
    case a: Fragment.StructuredText => a.blocks.collect { case b: Fragment.StructuredText.Block.Image => b.view }.map(v => Fragment.Image(v))
    case _                          => Nil
  }

  def getImage(field: String, view: String): Option[Fragment.Image.View] = get(field).flatMap {
    case a: Fragment.Image => a.getView(view)
    case a: Fragment.StructuredText if view == "main" => getImage(field).map(_.main)
    case _ => None
  }

  def getAllImages(field: String, view: String): Seq[Fragment.Image.View] = getAll(field).flatMap {
    case a: Fragment.Image => a.getView(view).toSeq
    case a: Fragment.StructuredText if view == "main" => getAllImages(field).map(_.main)
    case _ => Nil
  }

  def getStructuredText(field: String): Option[Fragment.StructuredText] = get(field).flatMap {
    case a: Fragment.StructuredText => Some(a)
    case _                          => None
  }

  def getHtml(field: String, linkResolver: DocumentLinkResolver): Option[String] = get(field).flatMap {
    case a: Fragment.StructuredText => Some(a.asHtml(linkResolver))
    case a: Fragment.Number         => Some(a.asHtml)
    case a: Fragment.Color          => Some(a.asHtml)
    case a: Fragment.Text           => Some(a.asHtml)
    case a: Fragment.Date           => Some(a.asHtml)
    case a: Fragment.Embed          => Some(a.asHtml)
    case a: Fragment.Image          => Some(a.asHtml)
    case a: Fragment.WebLink        => Some(a.asHtml)
    case a: Fragment.MediaLink      => Some(a.asHtml)
    case a: Fragment.DocumentLink   => Some(a.asHtml(linkResolver))
    case a: Fragment.Group          => Some(a asHtml linkResolver)
  }

  def getText(field: String): Option[String] = get(field).flatMap {
    case a: Fragment.StructuredText => Some(a.blocks.collect { case b: Fragment.StructuredText.Block.Text => b.text }.mkString("\n")).filterNot(_.isEmpty)
    case a: Fragment.Number         => Some(a.value.toString)
    case a: Fragment.Color          => Some(a.hex)
    case a: Fragment.Text           => Some(a.value).filterNot(_.isEmpty)
    case a: Fragment.Date           => Some(a.value.toString)
    case _                          => None
  }

  def getColor(field: String): Option[Fragment.Color] = get(field).flatMap {
    case a: Fragment.Color => Some(a)
    case _                 => None
  }

  def getNumber(field: String): Option[Fragment.Number] = get(field).flatMap {
    case a: Fragment.Number => Some(a)
    case _                  => None
  }

  def getDate(field: String): Option[Fragment.Date] = get(field).flatMap {
    case a: Fragment.Date => Some(a)
    case _                => None
  }

  def getDate(field: String, pattern: String): Option[String] = get(field).flatMap {
    case a: Fragment.Date => Some(a.asText(pattern))
    case _                => None
  }

  def getNumber(field: String, pattern: String): Option[String] = getNumber(field).map(_.asText(pattern))

  def getBoolean(field: String): Boolean = get(field).flatMap {
    case a: Fragment.Text => Option(a.value.toLowerCase).collect {
      case "yes"  => true
      case "true" => true
    }
    case _ => None
  }.getOrElse(false)

  def getGroup(field: String): Option[Fragment.Group] = get(field).flatMap {
    case a: Fragment.Group => Some(a)
    case _                 => None
  }

  def asHtml(linkResolver: DocumentLinkResolver): String = fragments.keys.map { field =>
    s"""<section data-field="$field">${getHtml(field, linkResolver).getOrElse("")}</section>"""
  }.mkString("\n")

}

case class Document(
    id: String,
    typ: String,
    href: String,
    tags: Seq[String],
    slugs: Seq[String],
    fragments: Map[String, Fragment]) extends WithFragments {

  def slug: String = slugs.headOption.getOrElse("-")

  def isTagged(requiredTags: Seq[String]) = requiredTags.forall(tag => tags.contains(tag))
}

object Document {

  def parse(jsvalue: JsObject): Option[Fragment] = {
    (jsvalue \ "type").asOpt[String].flatMap {

      case "Image"          => Some(Fragment.Image.reader.map(identity[Fragment]))
      case "Color"          => Some(Fragment.Color.reader.map(identity[Fragment]))
      case "Number"         => Some(Fragment.Number.reader.map(identity[Fragment]))
      case "Date"           => Some(Fragment.Date.reader.map(identity[Fragment]))
      case "Text"           => Some(Fragment.Text.reader.map(identity[Fragment]))
      case "Select"         => Some(Fragment.Text.reader.map(identity[Fragment]))
      case "Embed"          => Some(Fragment.Embed.reader.map(identity[Fragment]))
      case "Link.web"       => Some(Fragment.WebLink.reader.map(identity[Fragment]))
      case "Link.document"  => Some(Fragment.DocumentLink.reader.map(identity[Fragment]))
      case "StructuredText" => Some(Fragment.StructuredText.reader.map(identity[Fragment]))
      case "Group"          => Some(Fragment.Group.reader.map(identity[Fragment]))

      case t                => None
    }.flatMap(_.reads(jsvalue \ "value").asOpt)
  }

  implicit def reader = (
    (__ \ "id").read[String] and
    (__ \ "href").read[String] and
    (__ \ "tags").read[Seq[String]] and
    (__ \ "slugs").read[Seq[String]] and
    (__ \ "type").read[String].flatMap[(String, Map[String, Fragment])] { typ =>
      (__ \ "data" \ typ).read[JsObject].map { data =>
        collection.immutable.ListMap(
          data.fields.map {
            case (key, json: JsObject) => parse(json).toList.map(fragment => (s"$typ.$key", fragment))
            case (key, jsons: JsArray) => jsons.value.zipWithIndex.collect {
              case (json: JsObject, i) => parse(json).toList.map(fragment => (s"$typ.$key[$i]", fragment))
              case _                   => Nil
            }.flatten
            case _ => Nil
          }.flatten: _*
        )
      }.map(data => (typ, data))
    }
  )((id, href, tags, slugs, typAndData) => Document(id, typAndData._1, href, tags, slugs, typAndData._2))

}

