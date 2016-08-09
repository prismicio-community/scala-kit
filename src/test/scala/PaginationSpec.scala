package io.prismic

import org.specs2.mutable._

import scala.concurrent.duration._
import scala.concurrent.{ Future, Await }

class PaginationSpec extends Specification {

  private def await[A](fua: Future[A]) = Await.result(fua, DurationInt(5).seconds)

  private lazy val api = await(Api.get("https://micro.prismic.io/api", cache = BuiltInCache()))

  private val ref = api.master.ref
  private def query(page: Int, pageSize: Int = 20) =
    await(api.forms("everything").ref(ref)
      .page(page).pageSize(pageSize).submit())

  private def orderedQuery(page: Int, pageSize: Int = 20) =
    await(api.forms("everything").ref(ref)
      .page(page).pageSize(pageSize).orderings("[my.docchapter.priority]").submit())

  "Pagination" should {
    "first page" in {
      val res = query(1)
      res.page must_== 1
      res.results.head.id must_== "UrDejAEAAFwMyrW9"
      res.results.size must_== 18
      res.resultsPerPage must_== 20
      res.resultsSize must_== 18
      res.totalPages must_== 1
      res.nextPage must_== None
      res.prevPage must_== None
    }
    "second page" in {
      val res = query(2)
      res.page must_== 2
      res.results.size must_== 0
      res.resultsPerPage must_== 20
      res.resultsSize must_== 0
      res.totalPages must_== 1
      res.nextPage must_== None
      res.prevPage must_== Some(s"https://micro.prismic.io/api/documents/search?ref=V6nx_CUAAM0bW-T6&page=1&pageSize=20")
    }
    "setting page size" in {
      val res = query(1, 7)
      res.page must_== 1
      res.results.size must_== 7
      res.resultsPerPage must_== 7
      res.resultsSize must_== 7
      res.totalPages must_== 3
      res.nextPage must_== Some(s"https://micro.prismic.io/api/documents/search?ref=V6nx_CUAAM0bW-T6&page=2&pageSize=7")
      res.prevPage must_== None
    }
  }
}
