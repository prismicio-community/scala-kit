package io.prismic

import io.netty.handler.codec.http.{HttpResponseStatus, QueryStringEncoder}
import scala.concurrent.Future

import PrismicJson._
import PrismicJsonProtocol._

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * A SearchForm represent a Form returned by the prismic.io API.
 *
 * These forms depend on the prismic.io repository, and can be filled and sent
 * as regular HTML forms.
 *
 * You may get a SearchForm instance through the [[io.prismic.Api.forms]] method.
 *
 * The SearchForm instance contains helper methods for each predefined form's fields.
 *
 * @example
 *   val form = api.forms('everything')
 *     .page(3)                   // specify the field 'page'
 *     .pageSize(20)              // specify the 'page_size' field
 *   val results = form.submit()  // submit the search form
 */
case class SearchForm(api: Api, form: Form, data: Map[String, Seq[String]]) {

  def set(field: String, value: String): SearchForm = form.fields.get(field).map { fieldDesc =>
    copy(data = data ++ Map(field -> (if (fieldDesc.multiple) data.getOrElse(field, Nil) ++ Seq(value) else Seq(value))))
  }.getOrElse(sys.error(s"Unknown field $field"))

  def set(field: String, value: Int): SearchForm = form.fields.get(field).map(_.`type`).map {
    case "Integer" => set(field, value.toString)
    case t         => sys.error(s"Cannot use a Int as value for the field $field of type $t")
  }.getOrElse(sys.error(s"Unknown field $field"))

  def ref(r: Ref): SearchForm = ref(r.ref)
  def ref(r: String): SearchForm = set("ref", r)

  def query(query: String) = {
    if (form.fields.get("q").map(_.multiple) == Some(true)) {
      set("q", query)
    }
    else {
      // Temporary Hack for backward compatibility
      def strip(q: String) = q.trim.drop(1).dropRight(1)
      copy(data = data ++ Map("q" -> Seq(s"""[${form.fields("q").default.map(strip).getOrElse("")}${strip(query)}]""")))
    }
  }

  /**
   * Build an "AND" query with all the predicates passed in parameter
   * @param predicates one or more Predicate
   * @return the SearchForm instance for chaining
   */
  def query(predicates: Predicate*): SearchForm = {
    this.query("[" + predicates.map(_.q).mkString + "]")
  }

  def page(p: Int) = set("page", p)

  def pageSize(p: Int) = set("pageSize", p)

  /**
   * Restrict the fragments to be return to the set of fields in parameter
   * @param fields
   * @return
   */
  def fetch(fields: Iterable[String]) = set("fetch", fields.mkString(","))

  /**
   * Add the requested fields to the DocumentLink objects within the results
   * @param fields
   * @return
   */
  def fetchLinks(fields: Iterable[String]) = set("fetchLinks", fields.mkString(","))

  /**
   *
   * @param o one or more string, containing the name of a field optionally followed by a space and "desc"
   * @return the SearchForm instance for chaining
   */
  def orderings(o: String*) = {
    o.headOption match {
      case None => this // noop
      case Some(first) if first.matches("""^\[.*\]$""") => set("orderings", first) // backward compatibility
      case _ => set("orderings", s"[${o.mkString(", ")}]") // normal usage
    }
  }

  def submit(): Future[Response] = {

    (form.method, form.enctype, form.action) match {
      case ("GET", "application/x-www-form-urlencoded", action) =>

        val url = {
          val encoder = new QueryStringEncoder(form.action)
          data.foreach {
            case (key, values) => values.foreach(value => encoder.addParam(key, value))
          }
          encoder.toString
        }

        api.cache.get(url).map { json =>
          Future.successful(json.convertTo[Response])
        }.getOrElse {
          HttpClient.getJson(url, proxy = api.proxy).map { resp =>
            resp.status match {
              case HttpResponseStatus.OK =>
                val json = resp.json

                resp.header("Cache-Control").foreach {
                  case Api.MaxAge(duration) => api.cache.set(url, (System.currentTimeMillis + duration.toLong * 1000, json))
                  case _                    =>
                }

                json.convertTo[Response]
              case error => sys.error(s"Http error(status:$error msg:${resp.status.reasonPhrase()} body:${resp.body}")
            }
          }
        }

      case _ => Future.failed {
        sys.error(s"Form type not supported")
      }
    }
  }

}

/**
 * Paginated response to a Prismic.io query. Note that you may not get all documents in the first page,
 * and may need to retrieve more pages or increase the page size.
 */
case class Response(
  results: Seq[Document],
  page: Int,
  resultsPerPage: Int,
  resultsSize: Int,
  totalResultsSize: Int,
  totalPages: Int,
  nextPage: Option[String],
  prevPage: Option[String])
