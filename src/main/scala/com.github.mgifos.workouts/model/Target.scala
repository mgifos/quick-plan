package com.github.mgifos.workouts.model

import io.circe.Json

sealed trait Target {
  def json: Json
}

case class HrZoneTarget(zone: Int) extends Target {
  override def json: Json =
    Json.obj(
      "targetType"     -> Json.obj("workoutTargetTypeId" -> Json.fromInt(4), "workoutTargetTypeKey" -> Json.fromString("heart.rate.zone")),
      "targetValueOne" -> Json.fromString(""),
      "targetValueTwo" -> Json.fromString(""),
      "zoneNumber"     -> Json.fromString(zone.toString)
    )
}

case class HrCustomTarget(from: Int, to: Int) extends Target {
  override def json: Json =
    Json.obj(
      "targetType"     -> Json.obj("workoutTargetTypeId" -> Json.fromInt(4), "workoutTargetTypeKey" -> Json.fromString("heart.rate.zone")),
      "targetValueOne" -> Json.fromInt(from),
      "targetValueTwo" -> Json.fromInt(to),
      "zoneNumber"     -> Json.Null
    )
}

case class PaceTarget(from: Pace, to: Pace) extends Target {
  override def json: Json =
    Json.obj(
      "targetType"     -> Json.obj("workoutTargetTypeId" -> Json.fromInt(6), "workoutTargetTypeKey" -> Json.fromString("pace.zone")),
      "targetValueOne" -> Json.fromDoubleOrNull(from.speed),
      "targetValueTwo" -> Json.fromDoubleOrNull(to.speed),
      "zoneNumber"     -> Json.Null
    )
}

case class PowerCustomTarget(from: Int, to: Int) extends Target {
  override def json: Json =
    Json.obj(
      "targetType"     -> Json.obj("workoutTargetTypeId" -> Json.fromInt(2), "workoutTargetTypeKey" -> Json.fromString("power.zone")),
      "targetValueOne" -> Json.fromInt(from),
      "targetValueTwo" -> Json.fromInt(to),
      "zoneNumber"     -> Json.Null
    )
}

case class CadenceCustomTarget(from: Int, to: Int) extends Target {
  override def json: Json =
    Json.obj(
      "targetType"     -> Json.obj("workoutTargetTypeId" -> Json.fromInt(3), "workoutTargetTypeKey" -> Json.fromString("cadence.zone")),
      "targetValueOne" -> Json.fromInt(from),
      "targetValueTwo" -> Json.fromInt(to),
      "zoneNumber"     -> Json.Null
    )
}

case class SpeedTarget(from: Speed, to: Speed) extends Target {
  override def json: Json =
    Json.obj(
      "targetType"     -> Json.obj("workoutTargetTypeId" -> Json.fromInt(5), "workoutTargetTypeKey" -> Json.fromString("speed.zone")),
      "targetValueOne" -> Json.fromDoubleOrNull(from.speed),
      "targetValueTwo" -> Json.fromDoubleOrNull(to.speed),
      "zoneNumber"     -> Json.Null
    )
}

object NoTarget extends Target {
  override def json: Json =
    Json.obj(
      "targetType"     -> Json.obj("workoutTargetTypeId" -> Json.fromInt(1), "workoutTargetTypeKey" -> Json.fromString("no.target")),
      "targetValueOne" -> Json.Null,
      "targetValueTwo" -> Json.Null,
      "zoneNumber"     -> Json.Null
    )
}

case class Pace(uom: DistanceUnit, exp: String) {
  def minutes: Int = exp.trim.takeWhile(_ != ':').toInt
  def seconds: Int = exp.trim.split(":").last.toInt

  /**
    * @return Speed in m/s
    */
  def speed: Double = uom.toMeters(1) / (minutes * 60 + seconds)
}

case class Speed(unit: DistanceUnit, exp: String) {

  /**
    * @return Speed in m/s
    */
  def speed: Double = unit.toMeters(exp.toDouble) / 3600
}

object Target {
  private val CadenceCustomRx = """^(\d{1,3})\s*-\s*(\d{1,3})\s*rpm$""".r
  private val HrZoneRx = """^z(\d)$""".r
  private val HrCustomRx = """^(\d{1,3})\s*-\s*(\d{1,3})\s*bpm$""".r
  private val PaceRangeRx = """^(\d{1,2}:\d{2})\s*-\s*(\d{1,2}:\d{2})\s*(mpk|mpm)?$""".r
  private val PowerCustomRx = """^(\d{1,3})\s*-\s*(\d{1,3})\s*W$""".r
  private val SpeedRangeRx = """^(\d{1,3}(\.\d{1})?)\s*-\s*(\d{1,3}(\.\d{1})?)\s*(kph|mph)?""".r

  def parse(x: String)(using msys: MeasurementSystem): Target = x.trim match {
    case CadenceCustomRx(from, to) => CadenceCustomTarget(from.toInt, to.toInt)
    case HrZoneRx(zone)            => HrZoneTarget(zone.toInt)
    case HrCustomRx(from, to)      => HrCustomTarget(from.toInt, to.toInt)
    case PowerCustomRx(from, to)   => PowerCustomTarget(from.toInt, to.toInt)
    case SpeedRangeRx(from, _, to, _, uom) =>
      val du = Option(uom).fold(msys.distance)(DistanceUnit.withSpeedUOM)
      SpeedTarget(Speed(du, from), Speed(du, to))
    case PaceRangeRx(from, to, uom) =>
      val du = Option(uom).fold(msys.distance)(DistanceUnit.withPaceUOM)
      PaceTarget(Pace(du, from), Pace(du, to))
    case raw => throw new IllegalArgumentException(s"'$raw' is not a valid target specification")
  }
}
