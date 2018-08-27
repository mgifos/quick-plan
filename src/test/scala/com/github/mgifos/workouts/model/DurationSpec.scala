package com.github.mgifos.workouts.model

import org.scalatest.{ FlatSpec, Matchers }
import com.github.mgifos.workouts.model.DistanceUnits._

class DurationSpec extends FlatSpec with Matchers {

  implicit val msys = MeasurementSystems.metric

  "Duration" should "parse correctly" in {

    a[IllegalArgumentException] should be thrownBy Duration.parse("")
    a[IllegalArgumentException] should be thrownBy Duration.parse("5")
    a[IllegalArgumentException] should be thrownBy Duration.parse("5k")
    a[IllegalArgumentException] should be thrownBy Duration.parse("5 k")
    a[IllegalArgumentException] should be thrownBy Duration.parse(":")
    a[IllegalArgumentException] should be thrownBy Duration.parse("1:")
    a[IllegalArgumentException] should be thrownBy Duration.parse("1:0")
    a[IllegalArgumentException] should be thrownBy Duration.parse("01: 00")
    a[IllegalArgumentException] should be thrownBy Duration.parse("00")
    a[IllegalArgumentException] should be thrownBy Duration.parse(":00")
    a[IllegalArgumentException] should be thrownBy Duration.parse("1234:00")
    a[IllegalArgumentException] should be thrownBy Duration.parse("2:40:15")

    Duration.parse("5km") should be(DistanceDuration(5, km))
    Duration.parse("10 mi") should be(DistanceDuration(10, mi))
    Duration.parse("7.5m") should be(DistanceDuration(7.5f, m))

    Duration.parse("1:00") should be(TimeDuration(minutes = 1))
    Duration.parse("123:00") should be(TimeDuration(minutes = 123))
  }
}
