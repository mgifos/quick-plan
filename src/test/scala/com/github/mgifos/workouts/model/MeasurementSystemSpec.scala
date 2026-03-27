package com.github.mgifos.workouts.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.github.mgifos.workouts.model.DistanceUnit.*

class MeasurementSystemSpec extends AnyFlatSpec with Matchers {

  "DistanceUnit.named" should "return the matching unit" in {
    DistanceUnit.named("km") shouldBe km
    DistanceUnit.named("mi") shouldBe mi
    DistanceUnit.named("m") shouldBe m
  }

  it should "throw NoSuchElementException for an unknown name" in {
    a[NoSuchElementException] should be thrownBy DistanceUnit.named("ft")
  }

  "DistanceUnit.withPaceUOM" should "return km for mpk" in {
    DistanceUnit.withPaceUOM("mpk") shouldBe km
  }

  it should "return mi for mpm" in {
    DistanceUnit.withPaceUOM("mpm") shouldBe mi
  }

  it should "throw for an unknown pace UOM" in {
    an[IllegalArgumentException] should be thrownBy DistanceUnit.withPaceUOM("mps")
  }

  "DistanceUnit.withSpeedUOM" should "return km for kph" in {
    DistanceUnit.withSpeedUOM("kph") shouldBe km
  }

  it should "return mi for mph" in {
    DistanceUnit.withSpeedUOM("mph") shouldBe mi
  }

  it should "throw for an unknown speed UOM" in {
    an[IllegalArgumentException] should be thrownBy DistanceUnit.withSpeedUOM("fps")
  }

  "DistanceUnit.toMeters" should "convert km correctly" in {
    km.toMeters(1.0) shouldBe 1000.0
  }

  it should "convert miles correctly" in {
    mi.toMeters(1.0) shouldBe 1609.344
  }

  it should "convert meters as identity" in {
    m.toMeters(5.0) shouldBe 5.0
  }

  "MeasurementSystem.named" should "return metric" in {
    MeasurementSystem.named("metric") shouldBe MeasurementSystem.metric
  }

  it should "return imperial" in {
    MeasurementSystem.named("imperial") shouldBe MeasurementSystem.imperial
  }

  it should "throw NoSuchElementException for an unknown name" in {
    a[NoSuchElementException] should be thrownBy MeasurementSystem.named("US")
  }

  "MeasurementSystem" should "use miles as the distance unit for imperial" in {
    MeasurementSystem.imperial.distance shouldBe mi
  }

  it should "use km as the distance unit for metric" in {
    MeasurementSystem.metric.distance shouldBe km
  }

  "Target.parse with imperial system" should "default pace UOM to miles" in {
    given MeasurementSystem = MeasurementSystem.imperial
    Target.parse("5:20-06:00") shouldBe PaceTarget(Pace(mi, "5:20"), Pace(mi, "06:00"))
  }

  it should "default speed UOM to mph" in {
    given MeasurementSystem = MeasurementSystem.imperial
    Target.parse("10-15") shouldBe SpeedTarget(Speed(mi, "10"), Speed(mi, "15"))
  }
}
