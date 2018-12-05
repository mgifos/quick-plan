package com.github.mgifos.workouts.model

import com.github.mgifos.workouts.model.DistanceUnits._
import org.scalatest.{ FlatSpec, Matchers }

class StepSpec extends FlatSpec with Matchers {

  implicit val msys = MeasurementSystems.metric

  "Step" should "parse correctly" in {

    a[IllegalArgumentException] should be thrownBy Step.parse("")
    a[IllegalArgumentException] should be thrownBy Step.parse("- warmup: 5km\n  - run: 10km\n  - recover: 100m")

    Step.parse("- warmup: 5km") should be(WarmupStep(DistanceDuration(5, km)))
    Step.parse("- run: 2km @ 5:00-4:50") should be(IntervalStep(DistanceDuration(2, km), Some(PaceTarget(Pace(msys.distance, "5:00"), Pace(msys.distance, "4:50")))))
    Step.parse("- bike: 2km @ 20-30 kph") should be(IntervalStep(DistanceDuration(2, km), Some(SpeedTarget(Speed(msys.distance, "20"), Speed(msys.distance, "30")))))
    Step.parse("- go: 20km @ 9:00-11:00") should be(IntervalStep(DistanceDuration(20, km), Some(PaceTarget(Pace(msys.distance, "9:00"), Pace(msys.distance, "11:00")))))
    Step.parse("- recover: 500m @z2") should be(RecoverStep(DistanceDuration(500, m), Some(HrZoneTarget(2))))
    Step.parse("- cooldown: 05:00") should be(CooldownStep(TimeDuration(minutes = 5)))
    Step.parse("- repeat: 3\n  - run: 10km\n  - recover: 100m") should be(RepeatStep(3, List(
      IntervalStep(DistanceDuration(10, km)),
      RecoverStep(DistanceDuration(100, m)))))
  }
}
