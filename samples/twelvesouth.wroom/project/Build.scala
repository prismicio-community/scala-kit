import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "twelvesouth"
  val appVersion      = "1.0-SNAPSHOT"

  val mandubianRepo = Seq(
    "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
    "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/raw/master/releases/"
  )

  val localRepo = Seq(
    Resolver.url("Local IVY2 Repository", url("file://"+Path.userHome.absolutePath+"/.ivy2/local"))(Resolver.ivyStylePatterns)
  )

  val appDependencies = Seq(
    "com.zenexity.wroom" %% "wroom-client-scala" % "0.1-SNAPSHOT"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    resolvers ++= localRepo ++ mandubianRepo

  )

}
