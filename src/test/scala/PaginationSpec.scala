package io.prismic

import org.specs2.mutable._

import scala.concurrent.duration._
import scala.concurrent.{ Future, Await }

class PaginationSpec extends Specification {

  private def await[A](fua: Future[A]) = Await.result(fua, DurationInt(2).seconds)

  private lazy val api = await(Api.get("https://lesbonneschoses.prismic.io/api", cache = BuiltInCache()))
  private def query(page: Int, pageSize: Int = 20) =
    await(api.forms("everything").ref(api.master).page(page).pageSize(pageSize).submit())

  "Pagination" should {
    "first page" in {
      val res = query(1)
      res.page must_== 1
      res.results.size must_== 20
      res.resultsPerPage must_== 20
      res.resultsSize must_== 20
      res.totalPages must_== 2
      res.nextPage must_== Some("https://lesbonneschoses.prismic.io/api/documents/search?ref=UkL0hcuvzYUANCrm&page=2&pageSize=20")
      res.prevPage must_== None
    }
    "second page" in {
      val res = query(2)
      res.page must_== 2
      res.results.size must_== 20
      res.resultsPerPage must_== 20
      res.resultsSize must_== 20
      res.totalPages must_== 2
      res.nextPage must_== None
      res.prevPage must_== Some("https://lesbonneschoses.prismic.io/api/documents/search?ref=UkL0hcuvzYUANCrm&page=1&pageSize=20")
    }
    "setting page size" in {
      val res = query(1, 7)
      res.page must_== 1
      res.results.size must_== 7
      res.resultsPerPage must_== 7
      res.resultsSize must_== 7
      res.totalPages must_== 6
      res.nextPage must_== Some("https://lesbonneschoses.prismic.io/api/documents/search?ref=UkL0hcuvzYUANCrm&page=2&pageSize=7")
      res.prevPage must_== None
    }
  }
}
