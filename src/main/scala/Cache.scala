package io.prismic

import play.api.libs.json._

trait Cache {
  def set(url: String, response: (Long, JsValue)): Unit 
  def get(url: String): Option[JsValue] 
}

object NoCache extends Cache {
  def set(url: String, response: (Long, JsValue)) {}
  def get(url: String): Option[JsValue] = None
}

case class BuiltInCache(maxDocuments: Int = 100) extends Cache {

  import org.apache.commons.collections.map.LRUMap
  import scala.collection.convert.Wrappers.JMapWrapper
  import scala.collection.mutable.SynchronizedMap

  class SafeLRUMap[K, V](val maxSize: Int) extends JMapWrapper[K, V](new LRUMap(maxSize).asInstanceOf[java.util.Map[K, V]]) with SynchronizedMap[K, V]

  private val cache = new SafeLRUMap[String,(Long, JsValue)](maxDocuments)

  def set(url: String, response: (Long, JsValue)) {
    cache.put(url, response)
  }

  def get(url: String): Option[JsValue] = {
    cache.get(url).collect {
      case (expiration: Long, json: JsValue) if expiration > System.currentTimeMillis => json
    }
  }
}
