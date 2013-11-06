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
      resolvers += "Sonatype releases" at "http://oss.sonatype.org/content/repositories/releases",

      libraryDependencies ++= Seq(
        "com.typesafe.play"       %% "play-iteratees"      % "2.2.0",
        "com.typesafe.play"       %% "play-json"           % "2.2.0",
        "com.ning"                %  "async-http-client"   % "1.7.6",
        "com.google.guava" % "guava" % "15.0",
        "com.google.code.findbugs" % "jsr305" % "2.0.1",
        "org.specs2" %% "specs2" % "2.3.1" % "test"
      )

    )
  )
}
