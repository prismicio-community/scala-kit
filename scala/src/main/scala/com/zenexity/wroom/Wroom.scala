package com.zenexity.wroom.client

import scala.concurrent._
import org.joda.time._

import play.api.libs.json._

object Wroom {
  def getApi(theBaseUrl: String, accept: String = "application/json")(implicit ctx: ExecutionContext): Future[Api] = {
    WS.url(theBaseUrl + "/api")
      .withHeaders("Accept" -> accept)
      .get()
      .flatMap { resp =>
        Json.fromJson[ApiData](resp.json)
            .map{ d => 
              Future.successful( 
                new Api {
                  override val baseUrl = theBaseUrl
                  override val data = d
                }
              ) 
            }
            .recoverTotal( e => Future.failed(new java.lang.RuntimeException("couldn't parse received API: "+e.toString)) )
      }
  }
}
