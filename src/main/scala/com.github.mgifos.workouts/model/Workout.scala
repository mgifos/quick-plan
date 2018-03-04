package com.github.mgifos.workouts.model

case class Workout(name: String, steps: Seq[Step] = Nil) {
  def withStep(step: Step): Workout = Workout(name, steps :+ step)
}

object Workout {

  private val WorkoutName = """^workout:\s([\w-]+)((\n\s*-\s[a-z]+:.*)*)$""".r
  private val NextStepRx = """^((-\s\w*:\s.*)((\n\s{1,}-\s.*)*))(([\s].*)*)$""".r

  def parse(x: String) = {
    def loop(w: Workout, steps: String): Workout = steps match {
      case NextStepRx(next, _, _, _, rest, _) =>
        val newWorkout = w.withStep(Step.parse(next.trim))
        if (rest.trim.isEmpty) newWorkout
        else loop(newWorkout, rest.trim)
      case _ => throw new IllegalArgumentException(s"Input string cannot be parsed to Workout: $steps")
    }
    x match {
      case WorkoutName(name, steps, _) =>
        loop(Workout(name), steps.trim)
      case _ => throw new IllegalArgumentException(s"Input string cannot be parsed to Workout: $x")
    }
  }
}
