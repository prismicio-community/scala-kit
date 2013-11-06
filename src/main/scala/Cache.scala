package io.prismic

import play.api.libs.json._

trait Cache {
  def set(url: String, response: (Long, JsValue)): Unit 
  def get(url: String): Option[JsValue] 
}

object NoCache extends Cache {
  def set(url: String, response: (Long, JsValue)): Unit = ()
  def get(url: String): Option[JsValue] = None
}

case class BuiltInCache(maxDocuments: Int = 100) extends Cache {

  import com.google.common.cache.{ CacheBuilder, Cache ⇒ GuavaCache }

  private type CacheKey = String
  private type Response = (Long, JsValue)

  private val cache: GuavaCache[CacheKey, Response] = 
    CacheBuilder.newBuilder().maximumSize(maxDocuments).build[CacheKey, Response]

  def set(url: String, response: Response): Unit = {
    cache.put(url, response)
  }

  def get(url: String): Option[JsValue] = {
    Option(cache.getIfPresent(url)).collect {
      case (expiration: Long, json: JsValue) if expiration > System.currentTimeMillis ⇒ json
    }
  }
}
