import com.typesafe.sbt.packager.SettingsHelper._
import ReleaseTransformations._

name := "quick-plan"

lazy val root = (project in file(".")).enablePlugins(
  JavaAppPackaging,
  UniversalDeployPlugin)

Compile / mainClass := Some("com.github.mgifos.workouts.Main")

scalaVersion := "3.8.2"

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

ThisBuild / scalacOptions += "-Wunused:all"

val http4sVersion = "0.23.31"
val circeVersion  = "0.14.15"

libraryDependencies ++= Seq(
  "ch.qos.logback"             %  "logback-classic"              % "1.5.32",
  "com.github.scopt"           %% "scopt"                        % "4.1.0",
  "com.github.tototoshi"       %% "scala-csv"                    % "2.0.0",
  "com.typesafe.scala-logging" %% "scala-logging"                % "3.9.5",
  "org.typelevel"              %% "cats-effect"                  % "3.7.0",
  "co.fs2"                     %% "fs2-core"                     % "3.12.2",
  "org.http4s"                 %% "http4s-ember-client"          % http4sVersion,
  "org.http4s"                 %% "http4s-circe"                 % http4sVersion,
  "io.circe"                   %% "circe-core"                   % circeVersion,
  "io.circe"                   %% "circe-generic"                % circeVersion,
  "io.circe"                   %% "circe-parser"                 % circeVersion,
  "org.scalatest"              %% "scalatest"                    % "3.2.19"  % Test,
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

Global / excludeLintKeys ++= Set(
  Universal / artifacts,
  Universal / configuration,
  Universal / pushRemoteCacheConfiguration / publishMavenStyle,
  Universal / remoteCachePom / pushRemoteCacheArtifact
)

maintainer := "Nikola Petkov <nikola.petkov@gmail.com>"