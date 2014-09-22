package io.prismic

import org.specs2.mutable._

import scala.concurrent.duration._
import scala.concurrent.{ Future, Await }

class QuerySpec extends Specification {

  "Query builder" should {

    "at predicate" in {
      Predicate.at("document.type", "blog-post").q must_== """[:d = at(document.type, "blog-post")]"""
    }

    "any predicate" in {
      Predicate.any("document.tags", Seq("Macaron", "Cupcakes")).q must_== """[:d = any(document.tags, ["Macaron","Cupcakes"])]"""
    }

    "similar predicate" in {
      Predicate.similar("somedocid", 10).q must_== """[:d = similar("somedocid", 10)]"""
    }

    "number lt" in {
      Predicate.lt("my.product.price", 4.2).q must_== """[:d = number.lt(my.product.price, 4.2)]"""
    }

    "number in range" in {
      Predicate.inRange("my.product.price", 2, 4).q must_== """[:d = number.inRange(my.product.price, 2, 4)]"""
    }

    "month after" in {
      Predicate.monthAfter("my.blog-post.publication-date", Month.April).q must_==
        """[:d = date.month-after(my.blog-post.publication-date, "April")]"""
    }

    "geopoint near" in {
      Predicate.near("my.store.coordinates", 40.689757, -74.0451453, 15).q must_==
        """[:d = geopoint.near(my.store.coordinates, 40.689757, -74.0451453, 15)]"""
    }

  }
}
