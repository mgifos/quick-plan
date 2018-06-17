package com.github.mgifos.workouts.model

import org.scalatest.{ FlatSpec, Matchers }
import com.github.mgifos.workouts.model.DistanceUnits._
import play.api.libs.json.Json

class WorkoutSpec extends FlatSpec with Matchers {

  /*
  running: run-fast
  - warmup: 10:00
  - repeat: 2
    - run: 1500m @ 4:30-5:00
    - recover: 01:30 @ z2
  - cooldown: lap-button
   */
  val testWO = "running: run-fast\n- warmup: 10:00\n- repeat: 2\n  - run: 1500m @ 4:30-5:00\n  - recover: 01:30 @ z2\n- cooldown: lap-button"

  "Workout" should "parse correctly" in {
    Workout.parseDef("") should be('left)
    Workout.parseDef("running") should be('left)
    Workout.parseDef("running:") should be('left)
    Workout.parseDef("running !") should be('left)
    Workout.parseDef("running run-fast") should be('left)
    Workout.parseDef(" running: run-fast") should be('left)

    Workout.parseDef(testWO) should be(
      Right(
        WorkoutDef("running", "run-fast", Seq(
          WarmupStep(TimeDuration(minutes = 10)),
          RepeatStep(2, Seq(
            IntervalStep(DistanceDuration(1500, m), Some(PaceTarget(Pace("4:30"), Pace("5:00")))),
            RecoverStep(TimeDuration(1, 30), Some(HrZoneTarget(2))))),
          CooldownStep(LapButtonPressed)))))
  }

  "Workout" should "parse various printable workout-names correctly" in {

    val testNames = Seq("abcz", "123 xyw", """abc!/+-@,?*;:_!\"#$%&/()=?*""")

    testNames.foreach { testName =>
      val x = Workout.parseDef(testWO.replace("run-fast", testName))
      println(x)
      x.right.get.name should be(testName)
    }
  }

  "Workout" should "dump json correctly" in {
    val is = getClass.getClassLoader.getResourceAsStream("run-fast.json")
    val expectJson = Json.parse(is)
    Workout.parseDef(testWO).map(_.json) should be(Right(expectJson))
  }

  "Workout" should "support cycling ws" in {
    val testBike = "cycling: cycle-test\n- warmup: 5:00\n- bike: 20km @ 20.0-100kph\n- cooldown: lap-button"
    Workout.parseDef(testBike) should be(
      Right(
        WorkoutDef("cycling", "cycle-test", Seq(
          WarmupStep(TimeDuration(minutes = 5)),
          IntervalStep(DistanceDuration(20, km), Some(SpeedTarget(KphSpeed("20.0"), KphSpeed("100")))),
          CooldownStep(LapButtonPressed)))))
  }
}
