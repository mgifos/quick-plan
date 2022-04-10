import com.typesafe.sbt.packager.SettingsHelper._
import ReleaseTransformations._

name := "quick-plan"

lazy val root = (project in file(".")).enablePlugins(
  JavaAppPackaging,
  UniversalDeployPlugin)

mainClass in Compile := Some("com.github.mgifos.workouts.Main")

scalaVersion := "2.12.15"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.github.scopt" %% "scopt" % "3.7.0",
  "com.github.tototoshi" %% "scala-csv" % "1.3.5",
  "com.typesafe.akka" %% "akka-http"   % "10.1.5",
  "com.typesafe.akka" %% "akka-stream" % "2.5.19",
  "com.typesafe.play" %% "play-json" % "2.6.11",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
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

makeDeploymentSettings(Universal, packageBin in Universal, "zip")