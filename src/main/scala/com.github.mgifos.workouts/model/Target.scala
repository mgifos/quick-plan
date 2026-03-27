package com.github.mgifos.workouts.model

import io.circe.Json

sealed trait Target {
  def json: Json
}

case class HrZoneTarget(zone: Int) extends Target {
  override def json: Json =
    Target.targetJson(4, "heart.rate.zone", Json.fromString(""), Json.fromString(""), Json.fromString(zone.toString))
}

case class HrCustomTarget(from: Int, to: Int) extends Target {
  override def json: Json =
    Target.targetJson(4, "heart.rate.zone", Json.fromInt(from), Json.fromInt(to), Json.Null)
}

case class PaceTarget(from: Pace, to: Pace) extends Target {
  override def json: Json =
    Target.targetJson(6, "pace.zone", Json.fromDoubleOrNull(from.speed), Json.fromDoubleOrNull(to.speed), Json.Null)
}

case class PowerCustomTarget(from: Int, to: Int) extends Target {
  override def json: Json =
    Target.targetJson(2, "power.zone", Json.fromInt(from), Json.fromInt(to), Json.Null)
}

case class CadenceCustomTarget(from: Int, to: Int) extends Target {
  override def json: Json =
    Target.targetJson(3, "cadence.zone", Json.fromInt(from), Json.fromInt(to), Json.Null)
}

case class SpeedTarget(from: Speed, to: Speed) extends Target {
  override def json: Json =
    Target.targetJson(5, "speed.zone", Json.fromDoubleOrNull(from.speed), Json.fromDoubleOrNull(to.speed), Json.Null)
}

object NoTarget extends Target {
  override def json: Json =
    Target.targetJson(1, "no.target", Json.Null, Json.Null, Json.Null)
}

case class Pace(uom: DistanceUnit, exp: String) {
  def minutes: Int = exp.trim.takeWhile(_ != ':').toInt
  def seconds: Int = exp.trim.split(":")(1).toInt

  /**
   * @return
   *   Speed in m/s
   */
  def speed: Double = uom.toMeters(1) / (minutes * 60 + seconds)
}

case class Speed(unit: DistanceUnit, exp: String) {

  /**
   * @return
   *   Speed in m/s
   */
  def speed: Double = unit.toMeters(exp.toDouble) / 3600
}

object Target {

  private[model] def targetJson(typeId: Int, typeKey: String, v1: Json, v2: Json, zone: Json): Json =
    Json.obj(
      "targetType" -> Json.obj(
        "workoutTargetTypeId" -> Json.fromInt(typeId),
        "workoutTargetTypeKey" -> Json.fromString(typeKey)
      ),
      "targetValueOne" -> v1,
      "targetValueTwo" -> v2,
      "zoneNumber" -> zone
    )

  private val CadenceCustomRx = """^(\d{1,3})\s*-\s*(\d{1,3})\s*rpm$""".r
  private val HrZoneRx = """^z(\d)$""".r
  private val HrCustomRx = """^(\d{1,3})\s*-\s*(\d{1,3})\s*bpm$""".r
  private val PaceRangeRx = """^(\d{1,2}:\d{2})\s*-\s*(\d{1,2}:\d{2})\s*(mpk|mpm)?$""".r
  private val PowerCustomRx = """^(\d{1,3})\s*-\s*(\d{1,3})\s*W$""".r
  private val SpeedRangeRx = """^(\d{1,3}(\.\d{1})?)\s*-\s*(\d{1,3}(\.\d{1})?)\s*(kph|mph)?""".r

  def parse(x: String)(using msys: MeasurementSystem): Target = x.trim match {
    case CadenceCustomRx(from, to) => CadenceCustomTarget(from.toInt, to.toInt)
    case HrZoneRx(zone) => HrZoneTarget(zone.toInt)
    case HrCustomRx(from, to) => HrCustomTarget(from.toInt, to.toInt)
    case PowerCustomRx(from, to) => PowerCustomTarget(from.toInt, to.toInt)
    case SpeedRangeRx(from, _, to, _, uom) =>
      val du = Option(uom).fold(msys.distance)(DistanceUnit.withSpeedUOM)
      SpeedTarget(Speed(du, from), Speed(du, to))
    case PaceRangeRx(from, to, uom) =>
      val du = Option(uom).fold(msys.distance)(DistanceUnit.withPaceUOM)
      PaceTarget(Pace(du, from), Pace(du, to))
    case raw => throw new IllegalArgumentException(s"'$raw' is not a valid target specification")
  }
}
