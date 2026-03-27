package com.github.mgifos.workouts.model

import io.circe.Json

trait Step {
  def `type`: String
  def typeId: Int
  def json(order: Int): Json
}

abstract class DurationStep(override val `type`: String, override val typeId: Int) extends Step {
  def duration: Duration
  def target: Option[Target]

  def json(order: Int): Json =
    Json
      .obj(
        "type" -> Json.fromString("ExecutableStepDTO"),
        "stepId" -> Json.Null,
        "stepOrder" -> Json.fromInt(order),
        "childStepId" -> Json.Null,
        "description" -> Json.Null,
        "stepType" -> Json
          .obj("stepTypeId" -> Json.fromInt(typeId), "stepTypeKey" -> Json.fromString(`type`))
      )
      .deepMerge(duration.json)
      .deepMerge(target.fold(NoTarget.json)(_.json))
}

case class WarmupStep(duration: Duration, target: Option[Target] = None)
    extends DurationStep("warmup", 1)

case class CooldownStep(duration: Duration, target: Option[Target] = None)
    extends DurationStep("cooldown", 2)

case class IntervalStep(duration: Duration, target: Option[Target] = None)
    extends DurationStep("interval", 3)

case class RecoverStep(duration: Duration, target: Option[Target] = None)
    extends DurationStep("recovery", 4)

case class RepeatStep(count: Int, steps: Seq[Step]) extends Step {
  override def `type` = "repeat"
  override def typeId = 6

  override def json(order: Int): Json =
    Json.obj(
      "stepId" -> Json.Null,
      "stepOrder" -> Json.fromInt(order),
      "stepType" -> Json
        .obj("stepTypeId" -> Json.fromInt(typeId), "stepTypeKey" -> Json.fromString("repeat")),
      "numberOfIterations" -> Json.fromInt(count),
      "smartRepeat" -> Json.fromBoolean(false),
      "childStepId" -> Json.fromInt(1),
      "workoutSteps" -> Json.fromValues(steps.zipWithIndex.map { case (s, i) => s.json(i + 1) }),
      "type" -> Json.fromString("RepeatGroupDTO")
    )
}

object Step {

  def parse(text: String)(using msys: MeasurementSystem): Step = {

    def loop(depth: Int)(x: String): Step = {

      val indent = depth * 2

      val StepRx = raw"""^(\s{$indent}-\s\w*:\s.*)(([\r\n]+\s{1,}-\s.*)*)$$""".r
      val StepHeader = raw"""^\s{$indent}-\s*(\w*):(.*)$$""".r
      val ParamsRx = """^([\w-\.:\s]+)\s*(@(.*))?$""".r

      def parseDurationStep(x: String)(using msys: MeasurementSystem): DurationStep = x match {
        case StepHeader(name, params) =>
          name match {
            case "warmup" => val (d, t) = expect(params); WarmupStep(d, t)
            case "run" | "bike" | "go" => val (d, t) = expect(params); IntervalStep(d, t)
            case "recover" => val (d, t) = expect(params); RecoverStep(d, t)
            case "cooldown" => val (d, t) = expect(params); CooldownStep(d, t)
            case _ => throw new IllegalArgumentException(s"'$name' is not a duration step type")
          }
        case _ => throw new IllegalArgumentException(s"Cannot parse duration step: $x")
      }

      def expect(x: String)(using msys: MeasurementSystem): (Duration, Option[Target]) =
        x.trim match {
          case ParamsRx(duration, _, target) =>
            val maybeTarget = Option(target).filter(_.trim.nonEmpty).map(Target.parse)
            (Duration.parse(duration.trim), maybeTarget)
          case raw => throw new IllegalArgumentException(s"Cannot parse step parameters $raw")
        }

      x match {
        case StepRx(header, subdef, _) if subdef.nonEmpty =>
          header match {
            case StepHeader(name, params) =>
              if (name != "repeat")
                throw new IllegalArgumentException(
                  s"'$name' cannot contain sub-steps, it must be 'repeat'"
                )
              val next = subdef.replaceFirst("[\n\r]*", "")
              val nextIndent = indent + 2
              val steps = next.split(raw"""[\n\r]{1,2}\s{$nextIndent}-""").toList match {
                case head :: tail => head :: tail.map(" " * nextIndent + "-" + _)
                case original => original
              }
              RepeatStep(params.trim.toInt, steps.map(loop(depth + 1)))
            case _ => throw new IllegalArgumentException(s"Cannot parse repeat step $header")
          }
        case StepRx(header, subdef, _) if subdef.isEmpty => parseDurationStep(header)
        case _ => throw new IllegalArgumentException(s"Cannot parse step:$x")
      }
    }

    loop(0)(text)
  }

}
