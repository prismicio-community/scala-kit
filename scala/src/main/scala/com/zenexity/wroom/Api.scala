package com.zenexity.wroom.client

import scala.concurrent._
import org.joda.time._

import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.concurrent.ExecutionContext.Implicits.global

trait Cache {
  def set(url: String, response: (Long, JsValue)): Unit = ()
  def get(url: String): Option[JsValue] = None
}

object NoCache extends Cache

case class BuiltInCache(maxDocuments: Int = 100) extends Cache {
  private val cache = new org.apache.commons.collections.map.LRUMap(maxDocuments)

  override def set(url: String, response: (Long, JsValue)): Unit = {
    cache.put(url, response)
  }

  override def get(url: String): Option[JsValue] = {
    Option(cache.get(url)).collect {
      case (expiration: Long, json: JsValue) if expiration > System.currentTimeMillis => json
    }
  }
}

case class Api(data: ApiData, cache: Cache) {

  def refs: Map[String, Ref] = data.refs.groupBy(_.label).mapValues(_.head)
  def bookmarks: Map[String, String] = data.bookmarks
  def forms: Map[String, SearchForm] = data.forms.mapValues(form => SearchForm(this, form, form.defaultData))

  def master: Ref = refs.values.collectFirst { case ref if ref.isMasterRef => ref }.getOrElse(sys.error("no master reference found"))

}

object Api {

  val AcceptJson = Map("Accept" -> Seq("application/json"))
  val MaxAge = """max-age\s*=\s*(\d+)""".r

  def get(url: String, cache: Cache = NoCache): Future[Api] = {
    CustomWS.url(url)
      .copy(headers = AcceptJson)
      .get()
      .map { resp =>
        resp.status match {
          case 200    => Api(resp.json.as[ApiData], cache)
          case error  => sys.error(s"Http error $error (${resp.statusText}")
        }
      }
  }

}

case class SearchForm(api: Api, form: Form, data: Map[String,String]) {

  def ref(r: Ref): SearchForm = ref(r.ref)
  def ref(r: String): SearchForm = copy(data = data ++ Map("ref" -> r))

  def query(query: String) = {
    def strip(q: String) = q.trim.drop(1).dropRight(1)
    copy(data = data ++ Map("q" -> (s"[${form.fields("q").default.map(strip).getOrElse("")}${strip(query)}]")))
  }

  def submit(): Future[Seq[Document]] = {
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



