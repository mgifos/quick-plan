package com.github.mgifos.workouts.model

import com.github.mgifos.workouts.model.DistanceUnits.DistanceUnit

sealed trait Duration

case class DistanceDuration(distance: Float, unit: DistanceUnit) extends Duration

object DistanceUnits extends Enumeration {
  type DistanceUnit = Value
  val km, mi, m = Value
}
case class TimeDuration(hours: Int = 0, minutes: Int = 0, seconds: Int = 0) extends Duration

object Duration {

  private val DistanceRx = """^(\d+([\.]\d+)?)\s*(km|mi|m)$""".r
  private val MinutesRx = """^(\d{1,3}):(\d{2})$""".r

  def parse(x: String): Duration = x match {
    case DistanceRx(quantity, _, unit) => DistanceDuration(quantity.toFloat, DistanceUnits.withName(unit))
    case MinutesRx(minutes, seconds) => TimeDuration(minutes = minutes.toInt, seconds = seconds.toInt)
    case _ => throw new IllegalArgumentException(s"Duration cannot be parsed $x")
  }
}

