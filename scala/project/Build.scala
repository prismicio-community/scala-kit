import sbt._
import Keys._

object BuildSettings {
  val buildName              = "wroom-client-scala"
  val buildOrganization      = "com.zenexity.wroom"
  val buildVersion           = "0.1-SNAPSHOT"
  val buildScalaVersion      = "2.10.0"
  val playVersion            = "2.1.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization    := buildOrganization,
    version         := buildVersion,
    scalaVersion    := buildScalaVersion
  )
}

object ApplicationBuild extends Build {

  val typesafeRepo = Seq(
    "Typesafe repository snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
    "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"
  )

  val mandubianRepo = Seq(
    "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
    "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/raw/master/releases/"
  )

  lazy val wroomScalaKit = Project(
    BuildSettings.buildName, file("."),
    settings = BuildSettings.buildSettings ++ Seq(
      //logLevel := Level.Debug,
      //ivyLoggingLevel := UpdateLogging.Full,
      scalacOptions ++= Seq(
        //"-Xlog-implicits",
        //"-deprecation",
        //"-feature"
      ),
      resolvers := typesafeRepo ++ mandubianRepo,
      libraryDependencies ++= Seq(
        "play"       %% "play-iteratees"     % BuildSettings.playVersion         ,
        "play"       %% "play-json"          % "2.2-SNAPSHOT"                    ,
        "com.ning"   % "async-http-client"  % "1.7.6" /*notTransitive ()*/       ,
        //.exclude("org.jboss.netty", "netty"),
        "org.specs2" %% "specs2"             % "1.13"                    % "test",
        "junit"       % "junit"              % "4.8"                     % "test"
      )
    )
  )
}
