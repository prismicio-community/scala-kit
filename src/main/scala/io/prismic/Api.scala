package io.prismic

import io.netty.handler.codec.http.HttpResponseStatus
import org.joda.time._

import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

/**
 * High-level entry point for communications with prismic.io API
 */
final class Api(
    data: ApiData,
    accessToken: Option[String],
    serverProxy: Option[ProxyServer],
    private[prismic] val cache: Cache,
    private[prismic] val logger: (Symbol, String) => Unit) {

  def refs: Map[String, Ref] = data.refs.groupBy(_.label).mapValues(_.head)
  def bookmarks: Map[String, String] = data.bookmarks
  def forms: Map[String, SearchForm] = data.forms.mapValues(form => SearchForm(this, form, form.defaultData))
  def master: Ref = refs.values.collectFirst { case ref if ref.isMasterRef => ref }.getOrElse(sys.error("no master reference found"))
  /**
   * Experiments exposed by prismic.io API
   */
  def experiments: Experiments = data.experiments
  /**
   * Shortcut to the current running experiment, if any
   */
  def experiment: Option[Experiment] = experiments.current
  def proxy: Option[ProxyServer] = serverProxy

  /**
   * Return the URL to display a given preview
   * @param token as received from Prismic server to identify the content to preview
   * @param linkResolver the link resolver to build URL for your site
   * @param defaultUrl the URL to default to return if the preview doesn't correspond to a document
   *                (usually the home page of your site)
   * @return a Future corresponding to the URL you should redirect the user to preview the requested change
   */
  def previewSession(token: String, linkResolver: DocumentLinkResolver, defaultUrl: String): Future[String] = {
    try {
      (for {
        tokenJson <- HttpClient.getJson(token).map(_.json)
        mainDocumentId = (tokenJson \ "mainDocument").as[String]
        results <- forms("everything").query(Predicate.at("document.id", mainDocumentId)).ref(token).submit()
        document = results.results.head
      } yield {
        linkResolver(document.asDocumentLink)
      }).recoverWith {
        case _ => Future.successful(defaultUrl)
      }
    } catch {
      case _: Exception => Future.successful(defaultUrl)
    }
  }

  def oauthInitiateEndpoint = data.oauthEndpoints._1
  def oauthTokenEndpoint = data.oauthEndpoints._2

  // Helpers

  /**
   * Do an "at" query
   * @param field the field to query, for example "my.post.title"
   * @param value the value to match
   * @return
   */
  def findBy(field: String,
             value: String,
             form: String = "everything",
             ref: Ref = master,
             page: Int = 1,
             pageSize: Int = 20
              ): Future[Seq[Document]] =
    forms(form).ref(ref).page(page).pageSize(pageSize).query(Predicate.at(field, value)).submit().map(_.results)

  /**
   * Search documents from a list of ids
   * @param values
   * @return
   */
  def findByIds(values: Seq[String],
                form: String = "everything",
                ref: Ref = master,
                page: Int = 1,
                pageSize: Int = 20
                 ): Future[Seq[Document]] =
    forms(form).ref(ref).page(page).pageSize(pageSize).query(Predicate.any("document.id", values)).submit().map(_.results)

  /**
   * Retrieve the document with the corresponding id
   * @param value
   * @return
   */
  def findById(value: String,
               form: String = "everything",
               ref: Ref = master,
               page: Int = 1,
               pageSize: Int = 20
                ): Future[Option[Document]] =
    findBy("document.id", value, form, ref, page, pageSize).map(_.headOption)

  /**
   * Retrieve the document with the corresponding uid
   * @param value
   * @return
   */
  def findByUid(value: String,
               form: String = "everything",
               ref: Ref = master,
               page: Int = 1,
               pageSize: Int = 20
                ): Future[Option[Document]] =
    findBy("document.uid", value, form, ref, page, pageSize).map(_.headOption)

}


/**
 * Instanciate an Api instance from a prismic.io API URL
 */
object Api {

  private[prismic] val MaxAge = """max-age\s*=\s*(\d+)""".r

  /**
   * Instantiate an Api instance from a prismic.io API URL
   */
  def get(endpoint: String,
          accessToken: Option[String] = None,
          proxy: Option[ProxyServer] = None,
          cache: Cache = Cache.defaultCache,
          logger: (Symbol, String) => Unit = { (_, _) => () }): Future[Api] = {
    val url = accessToken.map(token => s"$endpoint?access_token=$token").getOrElse(endpoint)
    cache.getOrSet(url, 5000L) {
      HttpClient.getJson(url, proxy = proxy).map { resp =>
        resp.status match {
          case HttpResponseStatus.OK => resp.json
          case HttpResponseStatus.UNAUTHORIZED => (resp.json \ "oauth_initiate").asOpt[String] match {
            case Some(u) if accessToken.isDefined =>
              throw InvalidToken("The provided access token is either invalid or expired", u)
            case Some(u) =>
              throw AuthorizationNeeded("You need to provide an access token to access this repository", u)
            case None =>
              throw UnexpectedError("Authorization error, but not URL was provided")
          }
          case err => throw UnexpectedError(s"Got an HTTP error $err (${resp.body})")
        }
      }
    }.map { json =>
      new Api(
        ApiData.reader.reads(json).getOrElse(sys.error(s"Error while parsing API document: $json")),
        accessToken,
        proxy,
        cache,
        logger)
    }
  }

}

/**
 * Represent a prismic.io reference, a fixed point in time.
 *
 * The references must be provided when accessing to any prismic.io resource
 * (except /api) and allow to assert that the URL you use will always
 * returns the same results.
 */
case class Ref(
  id: String,
  ref: String,
  label: String,
  isMasterRef: Boolean = false,
  scheduledAt: Option[DateTime] = None)

private[prismic] object Ref {

  implicit val reader = (
    (__ \ "id").read[String] and
    (__ \ "ref").read[String] and
    (__ \ "label").read[String] and
    ((__ \ "isMasterRef").read[Boolean] orElse Reads.pure(false)) and
    (__ \ "scheduledAt").readNullable[DateTime]
  )(Ref.apply _)

}

/**
 * A prismic.io document field metadata
 */
case class Field(`type`: String, multiple: Boolean, default: Option[String])

private[prismic] object Field {
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

private[prismic] object Form {
  implicit val reader = Json.reads[Form]
}

private[prismic] case class ApiData(
  refs: Seq[Ref],
  bookmarks: Map[String, String],
  types: Map[String, String],
  tags: Seq[String],
  forms: Map[String, Form],
  oauthEndpoints: (String, String),
  experiments: Experiments)

private[prismic] object ApiData {

  import Experiment.readsExperiment
  implicit val reader = (
    (__ \ 'refs).read[Seq[Ref]] and
    (__ \ 'bookmarks).read[Map[String, String]] and
    (__ \ 'types).read[Map[String, String]] and
    (__ \ 'tags).read[Seq[String]] and
    (__ \ 'forms).read[Map[String, Form]] and
    (
      (__ \ 'oauth_initiate).read[String] and
        (__ \ 'oauth_token).read[String] tupled
    ) and
      (__ \ 'experiments).readNullable[Experiments].map(_ getOrElse Experiments(Nil, Nil))
  )(ApiData.apply _)

}

