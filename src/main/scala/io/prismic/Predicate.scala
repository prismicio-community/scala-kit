package io.prismic

import io.prismic.Month.Month
import io.prismic.WeekDay.WeekDay
import org.joda.time.DateTime

object WeekDay extends Enumeration {
  type WeekDay = Value
  val Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday = Value
}

object Month extends Enumeration {
  type Month = Value
  val January, February, March, April, May, June, July, August, September, October, November, December = Value
}

trait QuerySerializer[T] {
  def serialize(value: T): String
}

object QuerySerializer {

  def apply[T](f: T => String) = new QuerySerializer[T] {
    override def serialize(value: T): String = f(value)
  }

  implicit val StringSerializer: QuerySerializer[String] = apply("\"" + _ + "\"")

  implicit val DateSerializer: QuerySerializer[DateTime] = apply(_.getMillis.toString)

  implicit val LongSerializer: QuerySerializer[Long] = apply(_.toString)

  implicit val IntSerializer: QuerySerializer[Int] = apply(_.toString)

  implicit val BigDecimalSerializer: QuerySerializer[BigDecimal] = apply(_.toString)

  implicit val WeekDaySerializer: QuerySerializer[WeekDay] = apply("\"" + _ + "\"")

  implicit val MonthSerializer: QuerySerializer[Month] = apply("\"" + _ + "\"")

  implicit def seqSerializer[T](implicit ps: QuerySerializer[T]) = new QuerySerializer[Seq[T]] {
    override def serialize(value: Seq[T]): String = "[" + value.map(ps.serialize).mkString(",") + "]"
  }

}

/**
 * A Prismic predicate. Examples:
 * Query.at("document.type", "article")
 * Query.dateBefore("document.created", new DateTime().minusMonth(1))
 *
 * See the helpers in the companion object.
 */
sealed trait Predicate {
  /**
   * @return the predicate serialized as a string
   */
  def q: String
}

object Predicate {

  import QuerySerializer._

  def apply[T](operator: String, fragment: String, values: T*)(implicit ps: QuerySerializer[T]) = new Predicate {
    override def q = s"""[:d = $operator($fragment, ${values.map(ps.serialize).mkString(", ")})]"""
  }

  def at(fragment: String, value: String) = apply("at", fragment, value)

  def any(fragment: String, values: Seq[String]) = apply("any", fragment, values)

  def fulltext(fragment: String, value: String) = apply("fulltext", fragment, value)

  def similar(documentId: String, maxResults: Long) = new Predicate {
    override def q = s"""[:d = similar("$documentId", $maxResults)]"""
  }

  def gt(fragment: String, lowerBound: BigDecimal) = apply("number.gt", fragment, lowerBound)

  def lt(fragment: String, upperBound: BigDecimal) = apply("number.lt", fragment, upperBound)

  def inRange(fragment: String, lowerBound: BigDecimal, upperBound: BigDecimal) = apply("number.inRange", fragment, lowerBound, upperBound)

  def dateBefore(fragment: String, before: DateTime) = apply("date.before", fragment, before)

  def dateAfter(fragment: String, after: DateTime) = apply("date.after", fragment, after)

  def dateBetween(fragment: String, before: DateTime, after: DateTime) = apply("date.after", fragment, before, after)

  def dayOfMonth(fragment: String, day: Int) = apply("date.day-of-month", fragment, day)

  def dayOfMonthAfter(fragment: String, day: Int) = apply("date.day-of-month-after", fragment, day)

  def dayOfMonthBefore(fragment: String, day: Int) = apply("date.day-of-month-before", fragment, day)

  def dayOfWeek(fragment: String, day: WeekDay) = apply("date.day-of-week", fragment, day)

  def dayOfWeekBefore(fragment: String, day: WeekDay) = apply("date.day-of-week-before", fragment, day)

  def dayOfWeekAfter(fragment: String, day: WeekDay) = apply("date.day-of-week-after", fragment, day)

  def month(fragment: String, month: Month) = apply("date.month", fragment, month)

  def monthBefore(fragment: String, month: Month) = apply("date.month-before", fragment, month)

  def monthAfter(fragment: String, month: Month) = apply("date.month-after", fragment, month)

  def dateYear(fragment: String, year: Int) = apply("date.year", fragment, year)

  def hour(fragment: String, hour: Int) = apply("date.hour", fragment, hour)

  def hourBefore(fragment: String, hour: Int) = apply("date.hour-before", fragment, hour)

  def hourAfter(fragment: String, hour: Int) = apply("date.hour-after", fragment, hour)

}
