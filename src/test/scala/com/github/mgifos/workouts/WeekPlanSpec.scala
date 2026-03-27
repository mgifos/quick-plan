package com.github.mgifos.workouts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.github.mgifos.workouts.model.*

class WeekPlanSpec extends AnyFlatSpec with Matchers {

  given msys: MeasurementSystem = MeasurementSystem.metric

  val runFast =
    "running: run-fast\n- warmup: 10:00\n- repeat: 2\n  - run: 1500m @ 4:30-5:00\n  - recover: 01:30 @ z2\n- cooldown: lap-button"
  val runSlow = "running: run-slow\n- warmup: 10:00\n- run: 5km @ z2\n- cooldown: lap-button"
  val testPlan =
    s"""1,"$runFast",,run-fast,,run-fast,,,\n2,,run-fast,"$runSlow",run-fast,,run-slow,,"""

  "WeekPlan" should "detect workout definitions and references properly" in {
    val plan = new WeeklyPlan(testPlan.getBytes)
    plan.get().flatten should have size 7
    plan.workouts should have size 2
  }

  it should "return an empty plan for an empty CSV" in {
    val plan = new WeeklyPlan("".getBytes)
    plan.workouts shouldBe empty
    plan.get() shouldBe empty
    plan.invalid() shouldBe empty
  }

  it should "skip rows whose first column is not a number" in {
    val csv = s"""header,col2,col3\n1,"$runFast",,,,,,,"""
    val plan = new WeeklyPlan(csv.getBytes)
    plan.workouts should have size 1
  }

  it should "skip rows with a blank first column" in {
    val csv = s"""  ,"$runFast",,,,,,,\n1,"$runSlow",,,,,,,"""
    val plan = new WeeklyPlan(csv.getBytes)
    plan.workouts should have size 1
  }

  it should "ignore empty cells within a week" in {
    val csv = s"""1,"$runFast",,,,,,, """
    val plan = new WeeklyPlan(csv.getBytes)
    plan.get().flatten should have size 1
    plan.workouts should have size 1
  }

  it should "resolve references to definitions from previous weeks" in {
    val csv = s"""1,"$runFast",,,,,,,\n2,run-fast,,,,,,,"""
    val plan = new WeeklyPlan(csv.getBytes)
    val refs = plan.get().flatten
    refs should have size 2
    refs.forall(_.name == "run-fast") shouldBe true
    plan.workouts should have size 1
  }

  it should "treat unresolvable text as a note and exclude it from get()" in {
    val csv = s"""1,rest day,,,,,,,"""
    val plan = new WeeklyPlan(csv.getBytes)
    plan.get().flatten shouldBe empty
    plan.workouts shouldBe empty
  }

  it should "report invalid workout definitions via invalid()" in {
    val badWorkout = "running: bad-workout\n- and: 5km"
    val csv = s"""1,"$badWorkout",,,,,,,"""
    val plan = new WeeklyPlan(csv.getBytes)
    plan.invalid() should have size 1
    plan.invalid().head shouldBe a[WorkoutStepFailure]
  }

  it should "report invalid workout definitions that cannot be parsed at definition level" in {
    val badDef = "running:"
    val csv = s"""1,"$badDef",,,,,,,"""
    val plan = new WeeklyPlan(csv.getBytes)
    plan.invalid() should have size 1
    plan.invalid().head shouldBe a[WorkoutDefFailure]
  }

  it should "handle a single-week plan correctly" in {
    val csv = s"""1,"$runFast","$runSlow",,,,,, """
    val plan = new WeeklyPlan(csv.getBytes)
    plan.workouts should have size 2
    plan.get().flatten should have size 2
  }

  it should "accumulate definitions across three or more weeks" in {
    val runEasy = "running: run-easy\n- run: 3km @ z1\n- cooldown: lap-button"
    val csv =
      s"""1,"$runFast",,,,,,,\n2,"$runSlow",,,,,,,\n3,"$runEasy",run-fast,run-slow,,,,,"""
    val plan = new WeeklyPlan(csv.getBytes)
    plan.workouts should have size 3
    plan.get().flatten should have size 5
  }

  it should "include WorkoutDef slots as refs in get()" in {
    val csv = s"""1,"$runFast",,,,,,,"""
    val plan = new WeeklyPlan(csv.getBytes)
    plan.get().flatten.head shouldBe WorkoutRef("run-fast")
  }
}
