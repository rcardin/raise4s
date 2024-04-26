inThisBuild(
  List(
    organization := "in.rcard",
    homepage     := Some(url("https://github.com/rcardin")),
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

lazy val core = project
  .settings(
    name := "raise4s",
    libraryDependencies ++= commonDependencies
  )

lazy val ox = project
  .settings(
    name := "raise4s-ox",
    libraryDependencies ++= commonDependencies ++ Seq(
      dependencies.ox
    )
  )
  .dependsOn(
    core
  )

ThisBuild / scalaVersion := "3.4.1"

lazy val dependencies =
  new {
    val scalatestVersion = "3.2.17"
    val oxVersion        = "0.1.0"
    val ox               = "com.softwaremill.ox" %% "core"      % oxVersion
    val scalatest        = "org.scalatest"       %% "scalatest" % scalatestVersion
  }

lazy val commonDependencies = Seq(
  dependencies.scalatest % Test
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.17" % Test
)
