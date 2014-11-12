package io.prismic

import org.joda.time._

import play.api.libs.json._

/**
 * Experiment variation exposed by prismic API
 * @param id technical prismic ID
 * @param ref queryable ref of the variation (similar to release ref)
 * @param label user set label
 */
case class Variation(
  id: String,
  ref: String,
  label: String)

private[prismic] object Variation {

  implicit val readsVariation = Json.reads[Variation]
}

/**
 * Experiment exposed by prismic API
 * @param id prismic experiment ID
 * @param googleId Google experiment ID, empty if experiment is a draft
 * @param name user set name
 * @param variations list of variations for this experiment
 */
case class Experiment(
  id: String,
  googleId: Option[String],
  name: String,
  variations: Seq[Variation])

object Experiment {

  import Variation.readsVariation
  private[prismic] implicit val readsExperiment = Json.reads[Experiment]

  @deprecated("Use Prismic.experimentsCookie", "1.2.9")
  val cookieName = Prismic.experimentsCookie
}

/**
 * All experiments exposed by prismic API
 * @param draft experiments in draft stage, i.e. not running, for preview purpose
 * @param running experiments in running stage, that will be presented to users
 */
case class Experiments(
    draft: Seq[Experiment],
    running: Seq[Experiment]) {

  /**
   * First running experiment. To be used as the current running experiment.
   */
  def current = running.headOption

  /**
   * All experiments, draft and running
   */
  lazy val all = draft ++ running

  /**
   * Get the current running experiment variation ref from a cookie content
   */
  def refFromCookie(cookie: String): Option[String] =
    Some(cookie.trim).filter(_.nonEmpty).map(_ split "%20") flatMap {
      case Array(expId, varIndexStr) => running find (_.googleId == Some(expId)) flatMap { exp =>
        parseIntOption(varIndexStr) flatMap exp.variations.lift map (_.ref)
      }
      case _ => None
    }

  private def parseIntOption(str: String): Option[Int] = try {
    Some(java.lang.Integer.parseInt(str))
  }
  catch {
    case e: NumberFormatException => None
  }
}

private[prismic] case object Experiments {

  val empty = Experiments(Nil, Nil)

  import Experiment.readsExperiment
  implicit val readsExperiments = Json.reads[Experiments]
}
