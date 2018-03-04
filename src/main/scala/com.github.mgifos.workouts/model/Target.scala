package com.github.mgifos.workouts.model

sealed trait Target

case class HrZoneTarget(zone: Int) extends Target
case class PaceTarget(from: Pace, to: Pace) extends Target

case class Pace(exp: String) {
  def minutes: Int = exp.trim.takeWhile(_ != ':').toInt
  def seconds: Int = exp.trim.split(":").last.toInt
}

object Target {
  private val HrZoneRx = """^z(\d)$""".r
  private val PaceRangeRx = """^(\d{1,2}:\d{2})\s*-\s*(\d{1,2}:\d{2})$""".r

  def parse(x: String): Target = x.trim match {
    case HrZoneRx(zone) => HrZoneTarget(zone.toInt)
    case PaceRangeRx(from, to) => PaceTarget(Pace(from), Pace(to))
    case _ => throw new IllegalArgumentException(s"Unknown target specification: $x")
  }
}