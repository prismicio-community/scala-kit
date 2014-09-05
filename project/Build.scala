import sbt._
import Keys._
import xerial.sbt._
import com.typesafe.sbt.pgp.PgpKeys
import Sonatype.SonatypeKeys._


object BuildSettings {

  val buildName = "scala-kit"
  val buildOrganization = "io.prismic"
  val buildVersion = Option(System.getProperty("version")).map(_.trim).getOrElse("1.0-SNAPSHOT")
  val buildScalaVersion = "2.11.1"

  val buildSettings = xerial.sbt.Sonatype.sonatypeSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    crossScalaVersions := Seq("2.10.4", "2.11.1"),
    scalacOptions := Seq("-deprecation", "-unchecked", "-feature"),
    pomExtra := {
      <url>https://github.com/prismicio/scala-kit</url>
        <licenses>
          <license>
            <name>Apache 2</name>
            <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
          </license>
        </licenses>
        <scm>
          <connection>scm:git:github.com/prismicio/scala-kit.git</connection>
          <developerConnection>scm:git:git@github.com:prismicio/scala-kit.git</developerConnection>
          <url>github.com/prismicio/scala-kit.git</url>
        </scm>
        <developers>
          <developer>
            <name>Prismic.io Team</name>
            <email>contact@prismic.io</email>
            <organization>Prismic.io</organization>
            <organizationUrl>http://prismic.io</organizationUrl>
          </developer>
        </developers>
    }
  )
}

object KitBuild extends Build {

  lazy val ScalaKit = Project(
    BuildSettings.buildName, file("."),
    settings = BuildSettings.buildSettings ++ Seq(
      scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
      scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", baseDirectory.value + "/root-doc.txt"),

      resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/",
      resolvers += "Sonatype releases" at "http://oss.sonatype.org/content/repositories/releases",
      sourceGenerators in Compile <+= (sourceManaged in Compile, version, scalaVersion, name) map { (d, v, sv, n) =>
        val file = d / "info.scala"
        IO.write(file, """package io.prismic
                         |object Info {
                         |  val version = "%s"
                         |  val scalaVersion = "%s"
                         |  val name = "%s"
                         |}
                         |""".stripMargin.format(v, sv, n))
        Seq(file)
      },
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-iteratees" % "2.3.1",
        "com.typesafe.play" %% "play-json" % "2.3.1",
        "com.typesafe.play" %% "play-ws" % "2.3.1",
        "com.ning" % "async-http-client" % "1.8.9",
        "commons-collections" % "commons-collections" % "3.2.1",
        "org.specs2" %% "specs2" % "2.3.13" % "test"
      )
    )
  )
}
