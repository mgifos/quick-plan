package com.github.mgifos.workouts.model

import com.github.mgifos.workouts.model.DistanceUnits.DistanceUnit
import play.api.libs.json.{ JsNull, JsObject, Json }

sealed trait Duration {
  def json: JsObject
}

case class DistanceDuration(distance: Float, unit: DistanceUnit) extends Duration {
  override def json: JsObject = Json.obj(
    "endCondition" -> Json.obj(
      "conditionTypeKey" -> "distance",
      "conditionTypeId" -> 3),
    "preferredEndConditionUnit" -> Json.obj(
      "unitKey" -> unit.fullName),
    "endConditionValue" -> unit.toMeters(distance),
    "endConditionCompare" -> JsNull,
    "endConditionZone" -> JsNull)
}

object DistanceUnits extends Enumeration {
  type DistanceUnit = DistVal
  val km = Value("km", "kilometer", _ * 1000F)
  val mi = Value("mi", "mile", _ * 1609.344F)
  val m = Value("m", "meter", _ * 1F)

  class DistVal(name: String, val fullName: String, val toMeters: (Float) => Float) extends Val(nextId, name)
  protected final def Value(name: String, fullName: String, toMeters: (Float) => Float): DistVal = new DistVal(name, fullName, toMeters)

  def named(name: String): DistVal = withName(name).asInstanceOf[DistVal]
}

case class TimeDuration(minutes: Int = 0, seconds: Int = 0) extends Duration {
  override def json: JsObject = Json.obj(
    "endCondition" -> Json.obj(
      "conditionTypeKey" -> "time",
      "conditionTypeId" -> 2),
    "preferredEndConditionUnit" -> JsNull,
    "endConditionValue" -> (minutes * 60 + seconds),
    "endConditionCompare" -> JsNull,
    "endConditionZone" -> JsNull)
}

object LapButtonPressed extends Duration {
  override def json: JsObject = Json.obj(
    "endCondition" -> Json.obj(
      "conditionTypeKey" -> "lap.button",
      "conditionTypeId" -> 1),
    "preferredEndConditionUnit" -> JsNull,
    "endConditionValue" -> JsNull,
    "endConditionCompare" -> JsNull,
    "endConditionZone" -> JsNull)
}

object Duration {

  private val DistanceRx = """^(\d+([\.]\d+)?)\s*(km|mi|m)$""".r
  private val MinutesRx = """^(\d{1,3}):(\d{2})$""".r

  def parse(x: String): Duration = x match {
    case DistanceRx(quantity, _, unit) => DistanceDuration(quantity.toFloat, DistanceUnits.named(unit))
    case MinutesRx(minutes, seconds) => TimeDuration(minutes = minutes.toInt, seconds = seconds.toInt)
    case "lap-button" => LapButtonPressed
    case _ => throw new IllegalArgumentException(s"Duration cannot be parsed $x")
  }
}

