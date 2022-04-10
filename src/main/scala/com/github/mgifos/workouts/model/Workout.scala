package com.github.mgifos.workouts.model

import play.api.libs.json.{JsValue, Json}
import Workout._

trait Workout {
  def json(): JsValue = Json.obj()
  def valid(): Boolean = true
}

case class WorkoutDef(sport: String, name: String, steps: Seq[Step] = Nil) extends Workout {
  def toRef: WorkoutRef = WorkoutRef(name)
  def withStep(step: Step): WorkoutDef = WorkoutDef(sport, name, steps :+ step)
  override def json(): JsValue =
    Json.obj(
      "sportType" -> Json.obj("sportTypeId" -> sportId(sport), "sportTypeKey" -> sportTypeKey(sport)),
      "workoutName" -> name,
      "workoutSegments" -> Json.arr(
        Json.obj(
          "segmentOrder" -> 1,
          "sportType" -> Json.obj("sportTypeId" -> sportId(sport), "sportTypeKey" -> sport),
          "workoutSteps" -> steps.zipWithIndex.map { case (s, i) => s.json(i + 1) }
        ))
    )
}

case class WorkoutDefFailure(`type`: String, original: String, cause: String) extends Workout {
  override def toString = s"""Possible workout definition that can't be parsed: "$original"\nCause: "$cause"\n-------------------------------------"""
  override def valid(): Boolean = false
}

case class WorkoutStepFailure(original: String, cause: String) extends Workout {
  override def toString = s"""Workout steps that can't be parsed: "$original"\nCause: "$cause"\n-------------------------------------"""
  override def valid(): Boolean = false
}

case class WorkoutRef(name: String) extends Workout

case class WorkoutNote(note: String) extends Workout

object Workout {

  private val WorkoutType = "(running|cycling|custom)"
  private val WorkoutHeader = raw"""^$WorkoutType?:\s*([\u0020-\u007F]+)(([\r\n]+\s*\-\s[a-z]+:.*)*)$$""".r
  private val NextStepRx = """^((-\s\w*:\s.*)(([\r\n]+\s{1,}-\s.*)*))(([\s].*)*)$""".r
  private val PossibleWorkoutHeader = raw"""^\s*$WorkoutType?\s*:\s*.*(([\r\n]+\s*.*)*)$$""".r

  def parse(text: String)(implicit msys: MeasurementSystems.MeasurementSystem): Workout = {
    def loop(w: WorkoutDef, steps: String): Workout = steps match {
      case NextStepRx(next, _, _, _, rest, _) =>
        try {
          val newWorkout = w.withStep(Step.parse(next.trim))
          if (rest.trim.isEmpty) newWorkout
          else loop(newWorkout, rest.trim)
        } catch {
          case ex: IllegalArgumentException => WorkoutStepFailure(text, ex.getMessage.trim)
        }
      case _ => WorkoutStepFailure(text, steps.trim)
    }
    text match {
      case WorkoutHeader(null, name, steps, _)  => loop(WorkoutDef(detectSport(steps), name), steps.trim)
      case WorkoutHeader(sport, name, steps, _) => loop(WorkoutDef(sport, name), steps.trim)
      case PossibleWorkoutHeader(t, _, cause)   => WorkoutDefFailure(`type` = t, text, if (cause == null) "" else cause.trim)
      case _                                    => WorkoutNote(text)
    }
  }

  def detectSport(steps: String): String = steps match {
    case x if x.contains("- run")  => "running"
    case x if x.contains("- bike") => "cycling"
    case _                         => "custom"
  }

  def sportId(sport: String) = sport match {
    case "running" => 1
    case "cycling" => 2
    case "custom"  => 3
    case _         => throw new IllegalArgumentException("Only running, cycling and 'custom' workouts are supported.")
  }

  def sportTypeKey(sport: String) = sport match {
    case "custom" => "other"
    case _        => sport
  }
}
