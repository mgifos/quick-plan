package com.github.mgifos.workouts.model

import org.scalatest.{ FlatSpec, Matchers }
import com.github.mgifos.workouts.model.DistanceUnits._

class WorkoutSpec extends FlatSpec with Matchers {

  "Workout" should "parse correctly" in {
    a[IllegalArgumentException] should be thrownBy Workout.parse("")
    a[IllegalArgumentException] should be thrownBy Workout.parse("workout")
    a[IllegalArgumentException] should be thrownBy Workout.parse("workout:")
    a[IllegalArgumentException] should be thrownBy Workout.parse("workout: !")
    a[IllegalArgumentException] should be thrownBy Workout.parse("workout run-fast")
    a[IllegalArgumentException] should be thrownBy Workout.parse(" workout: run-fast")
    /*
    workout: run-fast
    - warmup: 10:00
    - repeat: 2
      - run: 1500m @ z4
      - recover: 01:30 @ z2
    - cooldown: 5:00
     */
    val testWO = "workout: run-fast\n- warmup: 10:00\n- repeat: 2\n  - run: 1500m @ z4\n  - recover: 01:30 @ z2\n- cooldown: 5:00"
    Workout.parse(testWO) should be(
      Workout("run-fast", Seq(
        WarmupStep(TimeDuration(minutes = 10)),
        RepeatStep(2, Seq(
          RunStep(DistanceDuration(1500, m), Some(HrZoneTarget(4))),
          RecoverStep(TimeDuration(0, 1, 30), Some(HrZoneTarget(2))))),
        CooldownStep(TimeDuration(minutes = 5)))))
  }
}
