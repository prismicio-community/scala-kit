package io.prismic

import org.joda.time._

import play.api.libs.json._

/**
 * Experiment variation exposed by prismic.io API
 * @param id technical prismic.io ID
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
 * Experiment exposed by prismic.io API
 * @param id prismic.io experiment ID
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

  /**
   * Name of the cookie that prismic.io will use to store the current
   * experiment variation index.
   * The experiment.js file uses the same cookie name.
   */
  val cookieName = "io.prismic.experiment"
}

/**
 * All experiments exposed by prismic.io API
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
}

private[prismic] case object Experiments {

  val empty = Experiments(Nil, Nil)

  import Experiment.readsExperiment
  implicit val readsExperiments = Json.reads[Experiments]
}
