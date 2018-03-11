package com.github.mgifos.workouts.model

trait Workout

case class WorkoutDef(name: String, steps: Seq[Step] = Nil) extends Workout {
  def toRef: WorkoutRef = WorkoutRef(name)
  def withStep(step: Step): WorkoutDef = WorkoutDef(name, steps :+ step)
}

case class WorkoutRef(name: String) extends Workout

case class WorkoutNote(note: String) extends Workout

object Workout {

  private val WorkoutName = """^workout:\s([\w \-,;:\.@]+)((\n\s*\-\s[a-z]+:.*)*)$""".r
  private val NextStepRx = """^((-\s\w*:\s.*)((\n\s{1,}-\s.*)*))(([\s].*)*)$""".r

  def parseDef(x: String): Either[String, WorkoutDef] = {
    def loop(w: WorkoutDef, steps: String): Either[String, WorkoutDef] = steps match {
      case NextStepRx(next, _, _, _, rest, _) =>
        val newWorkout = w.withStep(Step.parse(next.trim))
        if (rest.trim.isEmpty) Right(newWorkout)
        else loop(newWorkout, rest.trim)
      case _ => Left(s"Input string cannot be parsed to Workout: $steps")
    }
    x match {
      case WorkoutName(name, steps, _) => loop(WorkoutDef(name), steps.trim)
      case _ => Left(s"Input string cannot be parsed to Workout: $x")
    }
  }

  def parseRef(x: String): WorkoutRef = x match {
    case WorkoutName(name, _, _) => WorkoutRef(name)
    case _ => WorkoutRef(x.trim)
  }
}
