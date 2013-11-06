package io.prismic

import play.api.libs.json.JsNumber

import org.specs2.mutable._

class CacheSpec extends Specification {

  "An empty cache" should {
    "return empty result" in {
      val cache = emptyCache
      cache get "/foo" must beEmpty
    }
    "set and get and new entry" in {
      val cache = emptyCache
      cache.set("/bar", (expireAt, JsNumber(42)))
      cache get "/bar" must beSome(JsNumber(42))
    }
  }
  "A normal cache" should {
    "get existing entry" in {
      val cache = normalCache
      cache get "/foo/2" must beSome(JsNumber(2))
    }
    "set and get and new entry" in {
      val cache = normalCache
      cache.set("/bar", (expireAt, JsNumber(42)))
      cache get "/bar" must beSome(JsNumber(42))
    }
  }
  "A full cache" should {
    "accept new entries" in {
      val cache = fullCache
      cache.set("/bar", (expireAt, JsNumber(42)))
      cache get "/bar" must beSome(JsNumber(42))
    }
    "discard old entries" in {
      val cache = fullCache
      cache.set("/bar", (expireAt, JsNumber(42)))
      cache get "/foo/1" must beNone
    }
  }

  def emptyCache = BuiltInCache(maxDocuments = 10)
  def normalCache = fill(emptyCache, 5)
  def fullCache = fill(emptyCache, 10)

  def fill(cache: Cache, nbDocuments: Int) = {
    (1 to nbDocuments) foreach { i => cache.set(s"/foo/$i", (expireAt, JsNumber(i))) }
    cache
  }

  def expireAt = System.currentTimeMillis + 1000

  var cache: Cache = emptyCache
}
