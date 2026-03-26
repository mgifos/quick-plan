import com.typesafe.sbt.packager.SettingsHelper._
import ReleaseTransformations._

name := "quick-plan"

lazy val root = (project in file(".")).enablePlugins(
  JavaAppPackaging,
  UniversalDeployPlugin)

Compile / mainClass := Some("com.github.mgifos.workouts.Main")

scalaVersion := "2.13.14"

val http4sVersion = "0.23.27"
val circeVersion  = "0.14.9"

libraryDependencies ++= Seq(
  "ch.qos.logback"             %  "logback-classic"              % "1.5.6",
  "com.github.scopt"           %% "scopt"                        % "4.1.0",
  "com.github.tototoshi"       %% "scala-csv"                    % "1.3.10",
  "com.typesafe.scala-logging" %% "scala-logging"                % "3.9.5",
  "org.typelevel"              %% "cats-effect"                  % "3.5.4",
  "co.fs2"                     %% "fs2-core"                     % "3.10.2",
  "org.http4s"                 %% "http4s-ember-client"          % http4sVersion,
  "org.http4s"                 %% "http4s-circe"                 % http4sVersion,
  "io.circe"                   %% "circe-core"                   % circeVersion,
  "io.circe"                   %% "circe-generic"                % circeVersion,
  "io.circe"                   %% "circe-parser"                 % circeVersion,
  "org.scalatest"              %% "scalatest"                    % "3.2.18"  % Test,
  "org.typelevel"              %% "cats-effect-testing-scalatest" % "1.5.0"  % Test
)

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion,
  pushChanges
)

makeDeploymentSettings(Universal, Universal / packageBin, "zip")
