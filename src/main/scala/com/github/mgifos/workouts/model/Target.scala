package com.github.mgifos.workouts.model

import play.api.libs.json.{JsNull, JsObject, Json}

sealed trait Target {
  def json: JsObject
}

case class HrZoneTarget(zone: Int) extends Target {
  override def json =
    Json.obj(
      "targetType" -> Json.obj("workoutTargetTypeId" -> 4, "workoutTargetTypeKey" -> "heart.rate.zone"),
      "targetValueOne" -> "",
      "targetValueTwo" -> "",
      "zoneNumber" -> zone.toString
    )
}

case class HrCustomTarget(from: Int, to: Int) extends Target {
  override def json =
    Json.obj(
      "targetType" -> Json.obj("workoutTargetTypeId" -> 4, "workoutTargetTypeKey" -> "heart.rate.zone"),
      "targetValueOne" -> from,
      "targetValueTwo" -> to,
      "zoneNumber" -> JsNull
    )
}

case class PaceTarget(from: Pace, to: Pace) extends Target {
  override def json =
    Json.obj(
      "targetType" -> Json.obj("workoutTargetTypeId" -> 6, "workoutTargetTypeKey" -> "pace.zone"),
      "targetValueOne" -> from.speed,
      "targetValueTwo" -> to.speed,
      "zoneNumber" -> JsNull
    )
}

case class PowerCustomTarget(from: Int, to: Int) extends Target {
  override def json =
    Json.obj(
      "targetType" -> Json.obj("workoutTargetTypeId" -> 2, "workoutTargetTypeKey" -> "power.zone"),
      "targetValueOne" -> from,
      "targetValueTwo" -> to,
      "zoneNumber" -> JsNull
    )
}

case class CadenceCustomTarget(from: Int, to: Int) extends Target {
  override def json =
    Json.obj(
      "targetType" -> Json.obj("workoutTargetTypeId" -> 3, "workoutTargetTypeKey" -> "cadence.zone"),
      "targetValueOne" -> from,
      "targetValueTwo" -> to,
      "zoneNumber" -> JsNull
    )
}

case class SpeedTarget(from: Speed, to: Speed) extends Target {
  override def json =
    Json.obj(
      "targetType" -> Json.obj("workoutTargetTypeId" -> 5, "workoutTargetTypeKey" -> "speed.zone"),
      "targetValueOne" -> from.speed,
      "targetValueTwo" -> to.speed,
      "zoneNumber" -> JsNull
    )
}

object NoTarget extends Target {
  override def json =
    Json.obj(
      "targetType" -> Json.obj("workoutTargetTypeId" -> 1, "workoutTargetTypeKey" -> "no.target"),
      "targetValueOne" -> JsNull,
      "targetValueTwo" -> JsNull,
      "zoneNumber" -> JsNull
    )
}

case class Pace(uom: DistanceUnits.DistanceUnit, exp: String) {
  def minutes: Int = exp.trim.takeWhile(_ != ':').toInt
  def seconds: Int = exp.trim.split(":").last.toInt

  /**
    * @return Speed in m/s
    */
  def speed: Double = uom.toMeters(1) / (minutes * 60 + seconds)
}

case class Speed(unit: DistanceUnits.DistanceUnit, exp: String) {

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

  def parse(x: String)(implicit msys: MeasurementSystems.MeasurementSystem): Target = x.trim match {
    case CadenceCustomRx(from, to) => CadenceCustomTarget(from.toInt, to.toInt)
    case HrZoneRx(zone)            => HrZoneTarget(zone.toInt)
    case HrCustomRx(from, to)      => HrCustomTarget(from.toInt, to.toInt)
    case PowerCustomRx(from, to)   => PowerCustomTarget(from.toInt, to.toInt)
    case SpeedRangeRx(from, _, to, _, uom) =>
      val du = Option(uom).fold(msys.distance)(DistanceUnits.withSpeedUOM)
      SpeedTarget(Speed(du, from), Speed(du, to))
    case PaceRangeRx(from, to, uom) =>
      val du = Option(uom).fold(msys.distance)(DistanceUnits.withPaceUOM)
      PaceTarget(Pace(du, from), Pace(du, to))
    case raw => throw new IllegalArgumentException(s"'$raw' is not a valid target specification")
  }
}
