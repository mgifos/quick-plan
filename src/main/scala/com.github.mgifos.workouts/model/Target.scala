package com.github.mgifos.workouts.model

import play.api.libs.json.{ JsNull, JsObject, Json }

sealed trait Target {
  def json: JsObject
}

case class HrZoneTarget(zone: Int) extends Target {
  override def json = Json.obj(
    "targetType" -> Json.obj(
      "workoutTargetTypeId" -> 4,
      "workoutTargetTypeKey" -> "heart.rate.zone"),
    "targetValueOne" -> "",
    "targetValueTwo" -> "",
    "zoneNumber" -> zone.toString)
}
case class PaceTarget(from: Pace, to: Pace) extends Target {
  override def json = Json.obj(
    "targetType" -> Json.obj(
      "workoutTargetTypeId" -> 6,
      "workoutTargetTypeKey" -> "pace.zone"),
    "targetValueOne" -> from.speed,
    "targetValueTwo" -> to.speed,
    "zoneNumber" -> JsNull)
}

object NoTarget extends Target {
  override def json = Json.obj(
    "targetType" -> Json.obj(
      "workoutTargetTypeId" -> 1,
      "workoutTargetTypeKey" -> "no.target"),
    "targetValueOne" -> JsNull,
    "targetValueTwo" -> JsNull,
    "zoneNumber" -> JsNull)
}

case class Pace(exp: String) {
  def minutes: Int = exp.trim.takeWhile(_ != ':').toInt
  def seconds: Int = exp.trim.split(":").last.toInt

  /**
   * @return Speed in m/s
   */
  def speed: Double = 1000D / (minutes * 60 + seconds)
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