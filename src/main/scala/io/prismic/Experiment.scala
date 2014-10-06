package io.prismic

import org.joda.time._

import play.api.libs.json._

// variation as exposed by the prismic API
case class Variation(
  id: String,
  ref: String,
  label: String)

object Variation {

  implicit val readsVariation = Json.reads[Variation]
}

// experiment as exposed by the prismic API
case class Experiment(
  id: String,
  googleId: Option[String],
  name: String,
  variations: Seq[Variation])

object Experiment {

  import Variation.readsVariation
  implicit val readsExperiment = Json.reads[Experiment]

  val cookieName = "io.prismic.experiment"
}

// experiments as exposed by the prismic API
case class Experiments(
  draft: Seq[Experiment],
  running: Seq[Experiment]) {

  def current = running.headOption

  lazy val all = running ++ draft
}

case object Experiments {

  val empty = Experiments(Nil, Nil)

  import Experiment.readsExperiment
  implicit val readsExperiments = Json.reads[Experiments]
}
