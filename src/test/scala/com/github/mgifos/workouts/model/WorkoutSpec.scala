package com.github.mgifos.workouts.model

import org.scalatest.{ FlatSpec, Matchers }
import com.github.mgifos.workouts.model.DistanceUnits._
import play.api.libs.json.Json

class WorkoutSpec extends FlatSpec with Matchers {

  /*
  workout: run-fast
  - warmup: 10:00
  - repeat: 2
    - run: 1500m @ 4:30-5:00
    - recover: 01:30 @ z2
  - cooldown: lap-button
   */
  val testWO = "workout: run-fast\n- warmup: 10:00\n- repeat: 2\n  - run: 1500m @ 4:30-5:00\n  - recover: 01:30 @ z2\n- cooldown: lap-button"

  /*
  workout: 7x1k IV
  - warmup: 2km @z2
  - repeat: 3
    - run: 100m
    - recover: 1:00
  - repeat: 7
    - run: 1000m @z5
    - recover: 400m
  - recover: 2km @z2
  - cooldown: lap-button
  */
  val testWorkout2Repeat = "workout: 7x1k IV\n- warmup: 2km @z2\n- repeat: 3\n  - run: 100m\n  - recover: 1:00\n- repeat: 7\n  - run: 1000m @z5\n  - recover: 400m\n- recover: 2km @z2\n- cooldown: lap-button"

  "Workout" should "parse correctly" in {
    Workout.parseDef("") should be('left)
    Workout.parseDef("workout") should be('left)
    Workout.parseDef("workout:") should be('left)
    Workout.parseDef("workout: !") should be('left)
    Workout.parseDef("workout run-fast") should be('left)
    Workout.parseDef(" workout: run-fast") should be('left)

    Workout.parseDef(testWO) should be(
      Right(
        WorkoutDef("run-fast", Seq(
          WarmupStep(TimeDuration(minutes = 10)),
          RepeatStep(2, Seq(
            RunStep(DistanceDuration(1500, m), Some(PaceTarget(Pace("4:30"), Pace("5:00")))),
            RecoverStep(TimeDuration(1, 30), Some(HrZoneTarget(2))))),
          CooldownStep(LapButtonPressed)))))

    Workout.parseDef(testWorkout2Repeat) should be(
      Right(
        WorkoutDef("7x1k IV", Seq(
          WarmupStep(DistanceDuration(2, km), Some(HrZoneTarget(2))),
          RepeatStep(3, Seq(
            RunStep(DistanceDuration(100, m)),
            RecoverStep(TimeDuration(1, 0)))),
          RepeatStep(7, Seq(
            RunStep(DistanceDuration(1000, m), Some(HrZoneTarget(5))),
            RecoverStep(DistanceDuration(400, m)))),
          RecoverStep(DistanceDuration(2, km), Some(HrZoneTarget(2))),
          CooldownStep(LapButtonPressed)))))
  }

  "Workout" should "parse various printable workout-names correctly" in {

    val testNames = Seq("abcz", "123 xyw", """abc!/+-@,?*;:_!\"#$%&/()=?*""")

    testNames.foreach { testName =>
      val x = Workout.parseDef(testWO.replace("run-fast", testName))
      x.right.get.name should be(testName)
    }
  }

  "Workout" should "dump json correctly" in {
    val is = getClass.getClassLoader.getResourceAsStream("run-fast.json")
    val expectJson = Json.parse(is)
    Workout.parseDef(testWO).map(_.json) should be(Right(expectJson))
  }
}
