package com.github.mgifos.workouts

package object model {

  object DistanceUnits extends Enumeration {
    type DistanceUnit = DistVal
    val km = Value("km", "kilometer", _ * 1000F)
    val mi = Value("mi", "mile", _ * 1609.344F)
    val m = Value("m", "meter", _ * 1F)

    class DistVal(name: String, val fullName: String, val toMeters: (Double) => Double) extends Val(nextId, name)
    protected final def Value(name: String, fullName: String, toMeters: (Double) => Double): DistVal = new DistVal(name, fullName, toMeters)

    def named(name: String): DistVal = withName(name).asInstanceOf[DistVal]
    def withPaceUOM(paceUom: String): DistVal = paceUom match {
      case "mpk" => km
      case "mpm" => mi
      case _     => throw new IllegalArgumentException(s"No such pace unit of measurement: '$paceUom'")
    }
    def withSpeedUOM(speedUom: String): DistVal = speedUom match {
      case "kph" => km
      case "mph" => mi
      case _     => throw new IllegalArgumentException(s"No such speed unit of measurement: '$speedUom'")
    }
  }

  object MeasurementSystems extends Enumeration {
    type MeasurementSystem = MSVal
    val imperial = Value("imperial", DistanceUnits.mi)
    val metric = Value("metric", DistanceUnits.km)

    class MSVal(name: String, val distance: DistanceUnits.DistanceUnit) extends Val(nextId, name)
    protected final def Value(name: String, distance: DistanceUnits.DistanceUnit): MSVal = new MSVal(name, distance)

    def named(name: String): MSVal = withName(name).asInstanceOf[MSVal]
  }
}
