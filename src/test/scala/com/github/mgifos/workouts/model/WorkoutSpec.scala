package com.github.mgifos.workouts.model

import com.github.mgifos.workouts.model.DistanceUnits._
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.Json

class WorkoutSpec extends WordSpec with Matchers {

  implicit val msys = MeasurementSystems.metric

  /*
  running: run-fast
  - warmup: 10:00
  - repeat: 2
    - run: 1500m @ 4:30-5:00
    - recover: 01:30 @ z2
  - cooldown: lap-button
   */
  val testWO = "running: run-fast\n- warmup: 10:00\n- repeat: 2\n  - run: 1500m @ 4:30-5:00\n  - recover: 01:30 @ z2\n- cooldown: lap-button"

  "Workout parser" should {

    "parse notes correctly" in {
      Workout.parse("") shouldBe a[WorkoutNote]
      Workout.parse("running") shouldBe a[WorkoutNote]
      Workout.parse("running !") shouldBe a[WorkoutNote]
      Workout.parse("running run-fast") shouldBe a[WorkoutNote]
    }

    "parse a workout definition correctly" in {
      Workout.parse(testWO) shouldBe WorkoutDef(
        "running",
        "run-fast",
        Seq(
          WarmupStep(TimeDuration(minutes = 10)),
          RepeatStep(
            2,
            Seq(
              IntervalStep(DistanceDuration(1500, m), Some(PaceTarget(Pace(msys.distance, "4:30"), Pace(msys.distance, "5:00")))),
              RecoverStep(TimeDuration(1, 30), Some(HrZoneTarget(2)))
            )
          ),
          CooldownStep(LapButtonPressed)
        )
      )
    }

    "parse various printable workout-names correctly" in {

      val testNames = Seq("abcz", "123 xyw", """abc!/+-@,?*;:_!\"#$%&/()=?*""")

      testNames.foreach { testName =>
        val x = Workout.parse(testWO.replace("run-fast", testName))
        x shouldBe a[WorkoutDef]
        x.asInstanceOf[WorkoutDef].name should be(testName)
      }
    }

    "parse cycling workouts" in {
      val testBike = "cycling: cycle-test\r\n- warmup: 5:00\n- bike: 20km @ 20.0-100kph\r- cooldown: lap-button"
      Workout.parse(testBike) shouldBe WorkoutDef(
        "cycling",
        "cycle-test",
        Seq(
          WarmupStep(TimeDuration(minutes = 5)),
          IntervalStep(DistanceDuration(20, km), Some(SpeedTarget(Speed(km, "20.0"), Speed(km, "100")))),
          CooldownStep(LapButtonPressed)
        )
      )
    }

    "validate on a workout definition level" in {
      Workout.parse("running:") shouldBe a[WorkoutDefFailure]
      Workout.parse("running: run-fast\nhm") shouldBe a[WorkoutDefFailure]
      Workout.parse("running: run\n-  warmup: 10:00") shouldBe a[WorkoutDefFailure]
    }

    "validate on a step level" in {
      Workout.parse("running: run\n- run: 5km 2+2") should matchPattern {
        case WorkoutStepFailure(_, "Cannot parse step parameters 5km 2+2") =>
      }
      Workout.parse("running: run\n- run: 10:00\n  - run: 1500m @ 4:30-5:00") should matchPattern {
        case WorkoutStepFailure(_, "'run' cannot contain sub-steps, it must be 'repeat'") =>
      }
      Workout.parse("running: run\n- and: fail at this step") should matchPattern {
        case WorkoutStepFailure(_, "'and' is not a duration step type") =>
      }
      Workout.parse("running: run\n- run: 10km @ 20x") should matchPattern {
        case WorkoutStepFailure(_, "'20x' is not a valid target specification") =>
      }
    }

    "parse nested repeats" in {
      val testNested =
        "running: nested repeats\n- warmup: 20:00\n- repeat: 2\n  - repeat: 4\n    - run: 0.6km @ 4:25-4:15\n    - recover: 1:00\n  - recover: 05:00\n- cooldown: lap-button"
      Workout.parse(testNested) shouldBe WorkoutDef(
        "running",
        "nested repeats",
        Seq(
          WarmupStep(TimeDuration(20)),
          RepeatStep(
            count = 2,
            Seq(
              RepeatStep(count = 4,
                         Seq(IntervalStep(DistanceDuration(0.6f, km), Some(PaceTarget(Pace(km, "4:25"), Pace(km, "4:15")))),
                             RecoverStep(TimeDuration(1, 0)))),
              RecoverStep(TimeDuration(5, 0))
            )
          ),
          CooldownStep(LapButtonPressed)
        )
      )
    }

    "be able to detect workout type automatically" in {
      val test = Seq("run", "bike", "go").map(step => s": detect\n- $step: 2km")
      test.map(Workout.parse(_).asInstanceOf[WorkoutDef].sport) shouldBe Seq("running", "cycling", "custom")
    }
  }

  "Workout should" should {
    "dump json correctly" in {
      val is = getClass.getClassLoader.getResourceAsStream("run-fast.json")
      val expectJson = Json.parse(is)
      Workout.parse(testWO).json() should be(expectJson)
    }
  }

}
