inThisBuild(List(
  organization := "in.rcard",
  homepage := Some(url("https://github.com/rcardin")),
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
))

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

name := "raise4s"
//version := "0.0.1-SNAPSHOT"
scalaVersion := "3.4.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)