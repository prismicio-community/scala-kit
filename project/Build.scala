import sbt._
import Keys._

object BuildSettings {

  val buildName              = "scala-kit"
  val buildOrganization      = "io.prismic"
  val buildVersion           = Option(System.getProperty("version")).map(_.trim).getOrElse("1.0-SNAPSHOT")
  val buildScalaVersion      = "2.10.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization    := buildOrganization,
    version         := buildVersion,
    scalaVersion    := buildScalaVersion
  )
}

object KitBuild extends Build {

  lazy val ScalaKit = Project(
    BuildSettings.buildName, file("."),
    settings = BuildSettings.buildSettings ++ Seq(

      resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/",

      libraryDependencies ++= Seq(
        "com.typesafe.play"       %% "play-iteratees"      % "2.2.1",
        "com.typesafe.play"       %% "play-json"           % "2.2.1",
        "com.ning"                %  "async-http-client"   % "1.7.6",
        "commons-collections"     %  "commons-collections" % "3.2.1"
      )

    )
  )
}
