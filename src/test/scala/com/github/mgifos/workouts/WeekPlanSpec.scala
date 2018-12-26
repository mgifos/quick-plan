package com.github.mgifos.workouts

import com.github.mgifos.workouts.model._
import org.scalatest.{FlatSpec, Matchers}

class WeekPlanSpec extends FlatSpec with Matchers {

  implicit val msys = MeasurementSystems.metric

  val runFast = "running: run-fast\n- warmup: 10:00\n- repeat: 2\n  - run: 1500m @ 4:30-5:00\n  - recover: 01:30 @ z2\n- cooldown: lap-button"
  val runSlow = "running: run-slow\n- warmup: 10:00\n- run: 5km @ z2\n- cooldown: lap-button"
  val testPlan = s"""1,"$runFast",,run-fast,,run-fast,,,\n2,,run-fast,"$runSlow",run-fast,,run-slow,,"""

  "WeekPlan" should "detect workout definitions and references properly" in {
    val plan = new WeeklyPlan(testPlan.getBytes)
    plan.get.flatten should have size 7
    plan.workouts should have size 2
  }
}
