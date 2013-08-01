package io.prismic

import play.api.libs.json._

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