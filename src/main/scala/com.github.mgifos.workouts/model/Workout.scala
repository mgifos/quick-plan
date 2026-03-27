package com.github.mgifos.workouts.model

import io.circe.Json

import Workout.*

sealed trait Workout {
  def json: Json = Json.obj()
  def valid: Boolean = true
}

case class WorkoutDef(sport: String, name: String, steps: Seq[Step] = Nil) extends Workout {
  def toRef: WorkoutRef = WorkoutRef(name)
  def withStep(step: Step): WorkoutDef = WorkoutDef(sport, name, steps :+ step)
  override def json: Json = {
    val (id, key) = sportInfo(sport)
    Json.obj(
      "sportType" -> Json.obj(
        "sportTypeId" -> Json.fromInt(id),
        "sportTypeKey" -> Json.fromString(key)
      ),
      "workoutName" -> Json.fromString(name),
      "workoutSegments" -> Json.arr(
        Json.obj(
          "segmentOrder" -> Json.fromInt(1),
          "sportType" -> Json.obj(
            "sportTypeId" -> Json.fromInt(id),
            "sportTypeKey" -> Json.fromString(sport)
          ),
          "workoutSteps" -> Json.fromValues(steps.zipWithIndex.map { case (s, i) => s.json(i + 1) })
        )
      )
    )
  }
}

case class WorkoutDefFailure(`type`: String, original: String, cause: String) extends Workout {
  override def toString =
    s"""Possible workout definition that can't be parsed: "$original"\nCause: "$cause"\n-------------------------------------"""
  override def valid: Boolean = false
}

case class WorkoutStepFailure(original: String, cause: String) extends Workout {
  override def toString =
    s"""Workout steps that can't be parsed: "$original"\nCause: "$cause"\n-------------------------------------"""
  override def valid: Boolean = false
}

case class WorkoutRef(name: String) extends Workout

case class WorkoutNote(note: String) extends Workout

object Workout {

  private val WorkoutType = "(running|cycling|custom)"
  private val WorkoutHeader =
    raw"""^$WorkoutType?:\s*([\u0020-\u007F]+)(([\r\n]+\s*\-\s[a-z]+:.*)*)$$""".r
  private val NextStepRx = """^((-\s\w*:\s.*)(([\r\n]+\s{1,}-\s.*)*))(([\s].*)*)$""".r
  private val PossibleWorkoutHeader = raw"""^\s*$WorkoutType?\s*:\s*.*(([\r\n]+\s*.*)*)$$""".r

  def parse(text: String)(using msys: MeasurementSystem): Workout = {
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
      case WorkoutHeader(sportOrNull, name, steps, _) =>
        loop(WorkoutDef(Option(sportOrNull).getOrElse(detectSport(steps)), name), steps.trim)
      case PossibleWorkoutHeader(t, _, cause) =>
        WorkoutDefFailure(`type` = t, text, Option(cause).fold("")(_.trim))
      case _ => WorkoutNote(text)
    }
  }

  def detectSport(steps: String): String = steps match {
    case x if x.contains("- run") => "running"
    case x if x.contains("- bike") => "cycling"
    case _ => "custom"
  }

  def sportInfo(sport: String): (Int, String) = (sportId(sport), sportTypeKey(sport))

  def sportId(sport: String): Int = sport match {
    case "running" => 1
    case "cycling" => 2
    case "custom" => 3
    case _ =>
      throw new IllegalArgumentException(
        "Only running, cycling and 'custom' workouts are supported."
      )
  }

  def sportTypeKey(sport: String): String = sport match {
    case "custom" => "other"
    case _ => sport
  }
}
