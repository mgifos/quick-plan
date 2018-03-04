package com.github.mgifos.workouts.model

import org.scalatest.{ FlatSpec, Matchers }

class TargetSpec extends FlatSpec with Matchers {

  "Target" should "parse correctly" in {

    a[IllegalArgumentException] should be thrownBy Target.parse("")

    Target.parse("z1") should be(HrZoneTarget(1))
    Target.parse("z2") should be(HrZoneTarget(2))

    val paceTarget = Target.parse("5:20-04:30").asInstanceOf[PaceTarget]
    paceTarget.from.minutes should be(5)
    paceTarget.to.seconds should be(30)
    paceTarget should be(PaceTarget(Pace("5:20"), Pace("04:30")))
  }
}
