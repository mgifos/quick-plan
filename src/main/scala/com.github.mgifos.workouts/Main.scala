package com.github.mgifos.workouts

import java.nio.file.{ Files, Paths }
import java.time.LocalDate

import com.github.mgifos.workouts.model.WeeklyPlan

object Main extends App {

  val csvBytes = Files.readAllBytes(Paths.get("src/test/resources/ultra-80k-runnersworld.csv"))

  val wp = new WeeklyPlan(csvBytes)

  println(wp.workouts)

  val x = LocalDate.now()

  val it = wp.get.zipWithIndex.map {
    case (maybeWorkout, i) => x.plusDays(i) -> maybeWorkout
  }

  it.zipWithIndex.map { case (maybeScheduledWorkout, i) => s"day $i: $maybeScheduledWorkout" }.foreach(println)
}
