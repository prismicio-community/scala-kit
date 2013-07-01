package com.zenexity.wroom.client

import scala.concurrent._
import org.joda.time._

import play.api.libs.json._

case class Wroom(
  url: String,
  headers: Map[String, Seq[String]] = Map("Accept" -> Seq("application/json")),
  queryString: Map[String, Seq[String]] = Map()
){
  self => 

  def url(url: String) = copy(url = url)
  def withHeaders(hdrs: (String, String)*) = {
    val headers = hdrs.foldLeft(this.headers)((m, hdr) =>
      if (m.contains(hdr._1)) m.updated(hdr._1, m(hdr._1) :+ hdr._2)
      else (m + (hdr._1 -> Seq(hdr._2)))
    )
    this.copy(headers = headers)
  }

  def withQueryString(params: (String, String)*) = 
    this.copy(queryString = params.foldLeft(queryString) {
      case (m, (k, v)) => m + (k -> (v +: m.get(k).getOrElse(Nil)))
    })

  def api()(implicit ctx: ExecutionContext): Future[Api] = {
    CustomWS.url(s"$url/api")
      .copy(queryString = queryString)
      .copy(headers = headers)
      .get()
      .flatMap { resp =>
        resp.status match {
          case 200 =>
            Json.fromJson[ApiData](resp.json)
                .map{ d =>
                  Future.successful(
                    new Api {
                      override val baseUrl = url
                      override val queryString = self.queryString
                      override val headers = self.headers
                      override val data = d
                    }
                  )
                }
                .recoverTotal( e => Future.failed(new java.lang.RuntimeException("couldn't parse received API: "+e.toString)) )

          case error => Future.failed(new java.lang.RuntimeException(s"Http error(status:$error msg:${resp.statusText}"))
        }
      }
  }
}

object Wroom {
  def url(baseUrl: String) = Wroom(baseUrl)
}
