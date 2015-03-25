package io.prismic

import java.util.Collections
import org.apache.commons.collections4.map.LRUMap
import scala.collection.convert.Wrappers.JMapWrapper

import spray.json._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Cache prismic.io HTTP responses
 */
trait Cache {
  def set(key: String, data: (Long, JsValue)): Unit
  def get(key: String): Option[JsValue]
  def getOrSet(key: String, ttl: Long)(f: => Future[JsValue]): Future[JsValue]
  def isExpired(key: String): Boolean
}

object Cache {
  lazy val defaultCache = BuiltInCache(999)
}

/**
 * Do NOT cache prismic.io HTTP responses
 */
object NoCache extends Cache {
  def set(key: String, data: (Long, JsValue)) {}
  def get(key: String): Option[JsValue] = None
  def getOrSet(key: String, ttl: Long)(f: => Future[JsValue]) = f
  def isExpired(key: String): Boolean = true
}

/**
 * Default HTTP cache implementation, relying on apache in-memory LRUMap (recommended)
 */
case class BuiltInCache(maxDocuments: Int = 100) extends Cache {

  private val cache = JMapWrapper(Collections.synchronizedMap(new LRUMap[String, (Long, JsValue)](maxDocuments)))

  def set(key: String, data: (Long, JsValue)) {
    val (ttl, json) = data
    val expiration = System.currentTimeMillis + ttl
    cache.put(key, (expiration, json))
  }

  def get(key: String): Option[JsValue] = {
    val expired = isExpired(key)
    cache.get(key).collect {
      case (expiration: Long, json: JsValue) if !expired  => json
    }
  }

  def getOrSet(key: String, ttl: Long)(f: => Future[JsValue]): Future[JsValue] = {
    get(key).map(json => Future.successful(json)).getOrElse {
      f.map { json =>
        set(key, (ttl, json))
        json
      }
    }
  }

  def isExpired(key: String): Boolean = {
    cache.get(key) match {
      case Some((expiration, _)) =>
        expiration != 0 && expiration < System.currentTimeMillis
      case _ => false
    }
  }

}
