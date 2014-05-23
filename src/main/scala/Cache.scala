package io.prismic

import play.api.libs.json._
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
  def isPending(key: String): Boolean
}

sealed trait State
case object Pending extends State

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
  def isPending(key: String): Boolean = false
}

/**
 * Default HTTP cache implementation, relying on apache in-memory LRUMap (recommended)
 */
case class BuiltInCache(maxDocuments: Int = 100) extends Cache {

  import org.apache.commons.collections.map.LRUMap
  import scala.collection.convert.Wrappers.JMapWrapper
  import scala.collection.mutable.SynchronizedMap

  class SafeLRUMap[K, V](val maxSize: Int) extends JMapWrapper[K, V](new LRUMap(maxSize).asInstanceOf[java.util.Map[K, V]]) with SynchronizedMap[K, V]

  private val cache = new SafeLRUMap[String, (Long, JsValue)](maxDocuments)
  private val states = new java.util.concurrent.ConcurrentHashMap[String, State]()

  def set(key: String, data: (Long, JsValue)) {
    val (ttl, json) = data
    val expiration = System.currentTimeMillis + ttl
    cache.put(key, (expiration, json))
  }

  def get(key: String): Option[JsValue] = {
    val expired = isExpired(key)
    cache.get(key).collect {
      case (expiration: Long, json: JsValue) if !expired || (expired && isPending(key)) => json
    }
  }

  def getOrSet(key: String, ttl: Long)(f: => Future[JsValue]): Future[JsValue] = {
    get(key).map(json => Future.successful(json)).getOrElse {
      states.put(key, Pending)
      f.map { json =>
        set(key, (ttl, json))
        states.remove(key)
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

  def isPending(key: String): Boolean = states.get(key) == Pending
}
