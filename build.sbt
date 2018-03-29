name := "quick-plan"

version := "0.1"

lazy val root = (project in file(".")).enablePlugins(
  JavaAppPackaging,
  UniversalDeployPlugin)

mainClass in Compile := Some("com.github.mgifos.workouts.Main")

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.github.scopt" %% "scopt" % "3.7.0",
  "com.github.tototoshi" %% "scala-csv" % "1.3.5",
  "com.typesafe.akka" %% "akka-http"   % "10.1.0",
  "com.typesafe.akka" %% "akka-stream" % "2.5.11",
  "com.typesafe.play" %% "play-json" % "2.6.9",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)