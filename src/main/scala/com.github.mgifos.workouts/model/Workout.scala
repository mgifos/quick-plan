package com.github.mgifos.workouts.model

import play.api.libs.json.{ JsValue, Json }
import Workout._

trait Workout {
  def json: JsValue
}

case class WorkoutDef(sport: String, name: String, steps: Seq[Step] = Nil) extends Workout {
  def toRef: WorkoutRef = WorkoutRef(name)
  def withStep(step: Step): WorkoutDef = WorkoutDef(sport, name, steps :+ step)
  def json: JsValue = Json.obj(
    "sportType" -> Json.obj(
      "sportTypeId" -> sportId(sport),
      "sportTypeKey" -> sportTypeKey(sport)),
    "workoutName" -> name,
    "workoutSegments" -> Json.arr(
      Json.obj(
        "segmentOrder" -> 1,
        "sportType" -> Json.obj(
          "sportTypeId" -> sportId(sport),
          "sportTypeKey" -> sport),
        "workoutSteps" -> steps.zipWithIndex.map { case (s, i) => s.json(i + 1) })))
}

case class WorkoutRef(name: String) extends Workout {
  def json: JsValue = Json.obj()
}

case class WorkoutNote(note: String) extends Workout {
  def json: JsValue = Json.obj()
}

object Workout {

  private val WorkoutHeader = """^(running|cycling|custom):\s([\u0020-\u007F]+)(([\r\n]+\s*\-\s[a-z]+:.*)*)$""".r
  private val NextStepRx = """^((-\s\w*:\s.*)(([\r\n]+\s{1,}-\s.*)*))(([\s].*)*)$""".r

  def parseDef(x: String)(implicit msys: MeasurementSystems.MeasurementSystem): Either[String, WorkoutDef] = {
    def loop(w: WorkoutDef, steps: String): Either[String, WorkoutDef] = steps match {
      case NextStepRx(next, _, _, _, rest, _) =>
        val newWorkout = w.withStep(Step.parse(next.trim))
        if (rest.trim.isEmpty) Right(newWorkout)
        else loop(newWorkout, rest.trim)
      case _ => Left(s"Input string cannot be parsed to Workout: $steps")
    }
    x match {
      case WorkoutHeader(sport, name, steps, _) => loop(WorkoutDef(sport, name), steps.trim)
      case _ => Left(s"Input string cannot be parsed to Workout: $x")
    }
  }

  def parseRef(x: String): WorkoutRef = x match {
    case WorkoutHeader(_, name, _, _) => WorkoutRef(name)
    case _ => WorkoutRef(x.trim)
  }

  def sportId(sport: String) = sport match {
    case "running" => 1
    case "cycling" => 2
    case "custom" => 3
    case _ => throw new IllegalArgumentException("Only running, cycling and 'custom' workouts are supported.")
  }

  def sportTypeKey(sport: String) = sport match {
    case "custom" => "other"
    case _ => sport
  }
}
