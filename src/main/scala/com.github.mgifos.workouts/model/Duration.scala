package com.github.mgifos.workouts.model

import io.circe.Json

sealed trait Duration {
  def json: Json
}

case class DistanceDuration(distance: Float, unit: DistanceUnit) extends Duration {
  override def json: Json =
    Json.obj(
      "endCondition" -> Json.obj(
        "conditionTypeKey" -> Json.fromString("distance"),
        "conditionTypeId" -> Json.fromInt(3)
      ),
      "preferredEndConditionUnit" -> Json.obj("unitKey" -> Json.fromString(unit.fullName)),
      "endConditionValue" -> Json.fromDoubleOrNull(unit.toMeters(distance)),
      "endConditionCompare" -> Json.Null,
      "endConditionZone" -> Json.Null
    )
}

case class TimeDuration(minutes: Int = 0, seconds: Int = 0) extends Duration {
  override def json: Json =
    Json.obj(
      "endCondition" -> Json
        .obj("conditionTypeKey" -> Json.fromString("time"), "conditionTypeId" -> Json.fromInt(2)),
      "preferredEndConditionUnit" -> Json.Null,
      "endConditionValue" -> Json.fromInt(minutes * 60 + seconds),
      "endConditionCompare" -> Json.Null,
      "endConditionZone" -> Json.Null
    )
}

object LapButtonPressed extends Duration {
  override def json: Json =
    Json.obj(
      "endCondition" -> Json.obj(
        "conditionTypeKey" -> Json.fromString("lap.button"),
        "conditionTypeId" -> Json.fromInt(1)
      ),
      "preferredEndConditionUnit" -> Json.Null,
      "endConditionValue" -> Json.Null,
      "endConditionCompare" -> Json.Null,
      "endConditionZone" -> Json.Null
    )
}

object Duration {

  private val DistanceRx = """^(\d+([\.]\d+)?)\s*(km|mi|m)$""".r
  private val MinutesRx = """^(\d{1,3}):(\d{2})$""".r

  def parse(x: String): Duration = x match {
    case DistanceRx(quantity, _, uom) => DistanceDuration(quantity.toFloat, DistanceUnit.named(uom))
    case MinutesRx(minutes, seconds) =>
      TimeDuration(minutes = minutes.toInt, seconds = seconds.toInt)
    case "lap-button" => LapButtonPressed
    case _ => throw new IllegalArgumentException(s"Duration cannot be parsed $x")
  }
}
