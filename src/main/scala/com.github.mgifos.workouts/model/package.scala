package com.github.mgifos.workouts

package object model {

  enum DistanceUnit(val shortName: String, val fullName: String, val toMeters: Double => Double):
    case km extends DistanceUnit("km", "kilometer", _ * 1000.0)
    case mi extends DistanceUnit("mi", "mile", _ * 1609.344)
    case m extends DistanceUnit("m", "meter", _ * 1.0)

  object DistanceUnit:
    def named(name: String): DistanceUnit =
      DistanceUnit.values
        .find(_.shortName == name)
        .getOrElse(throw new NoSuchElementException(s"No distance unit: $name"))
    def withPaceUOM(paceUom: String): DistanceUnit = paceUom match
      case "mpk" => km
      case "mpm" => mi
      case _ => throw new IllegalArgumentException(s"No such pace unit of measurement: '$paceUom'")
    def withSpeedUOM(speedUom: String): DistanceUnit = speedUom match
      case "kph" => km
      case "mph" => mi
      case _ =>
        throw new IllegalArgumentException(s"No such speed unit of measurement: '$speedUom'")

  enum MeasurementSystem(val name: String, val distance: DistanceUnit):
    case imperial extends MeasurementSystem("imperial", DistanceUnit.mi)
    case metric extends MeasurementSystem("metric", DistanceUnit.km)

  object MeasurementSystem:
    def named(name: String): MeasurementSystem =
      MeasurementSystem.values
        .find(_.name == name)
        .getOrElse(throw new NoSuchElementException(s"No measurement system: $name"))
}
