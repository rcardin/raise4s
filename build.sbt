inThisBuild(
  List(
    organization := "in.rcard.raise4s",
    homepage     := Some(url("https://github.com/rcardin")),
    // Alternatively License.Apache2 see https://github.com/sbt/librarymanagement/blob/develop/core/src/main/scala/sbt/librarymanagement/License.scala
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "rcardin",
        "Riccardo Cardin",
        "riccardo DOT cardin AT gmail.com",
        url("https://github.com/rcardin/raise4s")
      )
    )
  )
)

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository                 := "https://s01.oss.sonatype.org/service/local"
sonatypeProfileName                := "in.rcard"

name := "raise4s"
//version := "0.0.1-SNAPSHOT"
val scala3Version = "3.4.2"
scalaVersion := scala3Version

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)

lazy val core = project
  .settings(
    name         := "core",
    scalaVersion := scala3Version,
    libraryDependencies ++= commonDependencies
  )

lazy val `cats-raise4s` = project
  .dependsOn(core)
  .settings(
    name         := "cats-raise4s",
    scalaVersion := scala3Version,
    libraryDependencies ++= commonDependencies ++ `cats-raised4sDependencies`
  )

lazy val raise4s = (project in file("."))
  .aggregate(core, `cats-raise4s`)
  .settings(
    scalaVersion := scala3Version
  )

lazy val dependencies =
  new {
    val scalatestVersion = "3.2.17"
    val scalatest        = "org.scalatest" %% "scalatest" % scalatestVersion
  }

lazy val commonDependencies = Seq(
  dependencies.scalatest % Test
)

lazy val `cats-raised4sDependencies` = Seq(
  "org.typelevel" %% "cats-core" % "2.12.0"
)
