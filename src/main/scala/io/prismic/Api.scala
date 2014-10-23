package io.prismic

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

  def oauthInitiateEndpoint = data.oauthEndpoints._1
  def oauthTokenEndpoint = data.oauthEndpoints._2
}


/**
 * Instanciate an Api instance from a prismic.io API URL
 */
object Api {

  private[prismic] val UserAgent = s"Prismic-${Info.name}/${Info.version} Scala/${Info.scalaVersion} JVM/${System.getProperty("java.version")}"
  private[prismic] val AcceptJson = Seq("Accept" -> "application/json")
  private[prismic] val MaxAge = """max-age\s*=\s*(\d+)""".r
  private[prismic] val httpClient: play.api.libs.ws.WSClient = {
    import play.api.libs.ws._
    import play.api.libs.ws.ning._
    new NingWSClient(
      new NingAsyncHttpClientConfigBuilder(DefaultWSClientConfig(userAgent = Some(UserAgent))).build()
    )
  }

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
      val req = httpClient.url(url).withHeaders(AcceptJson: _*)
      (proxy match {
        case Some(p) => req.withProxyServer(p.asPlayProxyServer)
        case _ => req
      }).get()
        .map { resp =>
          resp.status match {
            case 200 => resp.json
            case 401 => (resp.json \ "oauth_initiate").asOpt[String] match {
              case Some(u) if accessToken.isDefined =>
                throw InvalidToken("The provided access token is either invalid or expired", u)
              case Some(u) =>
                throw AuthorizationNeeded("You need to provide an access token to access this repository", u)
              case None =>
                throw UnexpectedError("Authorization error, but not URL was provided")
            }
            case err => throw UnexpectedError(s"Got an HTTP error $err (${resp.statusText})")
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
