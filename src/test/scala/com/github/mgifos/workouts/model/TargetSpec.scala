package com.github.mgifos.workouts.model

import org.scalatest.{ FlatSpec, Matchers }
import com.github.mgifos.workouts.model.DistanceUnits._

class TargetSpec extends FlatSpec with Matchers {

  implicit val msys = MeasurementSystems.metric

  "Target" should "parse correctly" in {

    a[IllegalArgumentException] should be thrownBy Target.parse("")

    Target.parse("z1") should be(HrZoneTarget(1))
    Target.parse("z2") should be(HrZoneTarget(2))

    val paceTarget = Target.parse("5:20-04:30").asInstanceOf[PaceTarget]
    paceTarget.from.minutes should be(5)
    paceTarget.to.seconds should be(30)
    paceTarget should be(PaceTarget(Pace(msys.distance, "5:20"), Pace(msys.distance, "04:30")))
  }

  "Target" should "parse pace UOMs correctly" in {
    val mpk = Target.parse("5:20-04:30 mpk").asInstanceOf[PaceTarget]
    mpk should be(PaceTarget(Pace(km, "5:20"), Pace(km, "04:30")))

    val mpm = Target.parse("4:20-05:30 mpm").asInstanceOf[PaceTarget]
    mpm should be(PaceTarget(Pace(mi, "4:20"), Pace(mi, "05:30")))
    mpm.from.speed should be(6.189784592848557D)

    a[IllegalArgumentException] should be thrownBy Target.parse("5:20-04:30 unknownUOM")
  }
}
