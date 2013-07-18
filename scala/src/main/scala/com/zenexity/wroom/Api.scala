package com.zenexity.wroom.client

import scala.concurrent._
import org.joda.time._

import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.concurrent.ExecutionContext.Implicits.global

case class Api(data: ApiData) {

  def refs: Map[String, Ref] = data.refs.groupBy(_.label).mapValues(_.head)
  def bookmarks: Map[String, String] = data.bookmarks
  def forms: Map[String, SearchForm] = data.forms.mapValues(f => SearchForm(f, f.defaultData))

  def master: Ref = refs.values.collectFirst { case ref if ref.isMasterRef => ref }.getOrElse(sys.error("no master reference found"))

}

object Api {

  val AcceptJson = Map("Accept" -> Seq("application/json"))

  def get(url: String): Future[Api] = {
    CustomWS.url(url)
      .copy(headers = AcceptJson)
      .get()
      .map { resp =>
        resp.status match {
          case 200    => Api(resp.json.as[ApiData])
          case error  => sys.error(s"Http error $error (${resp.statusText}")
        }
      }
  }

}


case class SearchForm(form: Form, data: Map[String,String]) {

  def ref(r: Ref): SearchForm = ref(r.ref)
  def ref(r: String): SearchForm = copy(data = data ++ Map("ref" -> r))
  def query(query: String) = {
    def strip(q: String) = q.trim.drop(1).dropRight(1)
    copy(data = data ++ Map("q" -> (s"[${form.fields("q").default.map(strip).getOrElse("")}${strip(query)}]")))
  }

  def submit(): Future[Seq[Document]] = {
    val method = form.method
    val enctype = form.enctype
    var holder = CustomWS.url(form.action)
    method match {
      case "GET" =>
        enctype match {
          case "application/x-www-form-urlencoded" =>
            holder = holder.copy(queryString = data.mapValues(v => Seq(v)))
          case _ => sys.error("enctype not managed")
        }
        // JSON for now
        holder = holder.copy(headers = Api.AcceptJson)
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



