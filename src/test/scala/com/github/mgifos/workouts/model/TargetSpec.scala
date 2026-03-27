package com.github.mgifos.workouts.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.mgifos.workouts.model.DistanceUnit.*
import io.circe.Json

class TargetSpec extends AnyFlatSpec with Matchers {

  given msys: MeasurementSystem = MeasurementSystem.metric

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
    mpm.from.speed should be(6.189784615384616D)

    a[IllegalArgumentException] should be thrownBy Target.parse("5:20-04:30 unknownUOM")
  }

  "Target" should "handle custom HR specification correctly" in {
    val hrcTarget = Target.parse("130-150 bpm").asInstanceOf[HrCustomTarget]
    hrcTarget should be(HrCustomTarget(130, 150))
    hrcTarget.json should be(
      Json.obj(
        "targetType"     -> Json.obj("workoutTargetTypeId" -> Json.fromInt(4), "workoutTargetTypeKey" -> Json.fromString("heart.rate.zone")),
        "targetValueOne" -> Json.fromInt(130),
        "targetValueTwo" -> Json.fromInt(150),
        "zoneNumber"     -> Json.Null
      ))
  }

  "Target" should "handle custom POWER specification correctly" in {
    val powTarget = Target.parse("230-250 W").asInstanceOf[PowerCustomTarget]
    powTarget should be(PowerCustomTarget(230, 250))
    powTarget.json should be(
      Json.obj(
        "targetType"     -> Json.obj("workoutTargetTypeId" -> Json.fromInt(2), "workoutTargetTypeKey" -> Json.fromString("power.zone")),
        "targetValueOne" -> Json.fromInt(230),
        "targetValueTwo" -> Json.fromInt(250),
        "zoneNumber"     -> Json.Null
      ))
  }

  "Target" should "handle custom CADENCE specification correctly" in {
    val cadenceTarget = Target.parse("80-90 rpm").asInstanceOf[CadenceCustomTarget]
    cadenceTarget should be(CadenceCustomTarget(80, 90))
    cadenceTarget.json should be(
      Json.obj(
        "targetType"     -> Json.obj("workoutTargetTypeId" -> Json.fromInt(3), "workoutTargetTypeKey" -> Json.fromString("cadence.zone")),
        "targetValueOne" -> Json.fromInt(80),
        "targetValueTwo" -> Json.fromInt(90),
        "zoneNumber"     -> Json.Null
      ))
  }
}
