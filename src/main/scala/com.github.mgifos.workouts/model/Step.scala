package com.github.mgifos.workouts.model

import play.api.libs.json.{ JsNull, JsObject, JsValue, Json }

trait Step {
  def `type`: String
  def typeId: Int
  def json(order: Int): JsValue
}

abstract class DurationStep(
  override val `type`: String,
  override val typeId: Int) extends Step {
  def duration: Duration
  def target: Option[Target]

  def json(order: Int): JsObject = Json.obj(
    "type" -> "ExecutableStepDTO",
    "stepId" -> JsNull,
    "stepOrder" -> order,
    "childStepId" -> JsNull,
    "description" -> JsNull,
    "stepType" -> Json.obj(
      "stepTypeId" -> typeId,
      "stepTypeKey" -> `type`)) ++ duration.json ++ target.fold(NoTarget.json)(_.json)
}

case class WarmupStep(duration: Duration, target: Option[Target] = None) extends DurationStep("warmup", 1)

case class CooldownStep(duration: Duration, target: Option[Target] = None) extends DurationStep("cooldown", 2)

case class IntervalStep(duration: Duration, target: Option[Target] = None) extends DurationStep("interval", 3)

case class RecoverStep(duration: Duration, target: Option[Target] = None) extends DurationStep("recovery", 4)

case class RepeatStep(count: Int, steps: Seq[Step]) extends Step {
  override def `type` = "repeat"
  override def typeId = 6

  override def json(order: Int) = Json.obj(
    "stepId" -> JsNull,
    "stepOrder" -> order,
    "stepType" -> Json.obj(
      "stepTypeId" -> typeId,
      "stepTypeKey" -> "repeat"),
    "numberOfIterations" -> count,
    "smartRepeat" -> false,
    "childStepId" -> 1,
    "workoutSteps" -> steps.zipWithIndex.map { case (s, i) => s.json(i + 1) },
    "type" -> "RepeatGroupDTO")
}

object Step {

  private val StepRx = """^(-\s\w*:\s.*)((\n\s{1,}-\s.*)*)$""".r
  private val StepHeader = """^\s*-\s*(\w*):(.*)$""".r
  private val ParamsRx = """^([\w-\.:\s]+)\s*(@(.*))?$""".r

  def parse(x: String): Step = x match {
    case StepRx(header, subSteps, _) if subSteps.nonEmpty => header match {
      case StepHeader(name, params) =>
        assert(name == "repeat", "must be 'repeat' if contains sub-steps")
        RepeatStep(params.trim.toInt, subSteps.trim.lines.toList.map(parseDurationStep))
      case _ => throw new IllegalArgumentException(s"Cannot parse repeat step $header")
    }
    case StepRx(header, "", null) => parseDurationStep(header)
    case _ => throw new IllegalArgumentException(s"Cannot parse step:$x")
  }

  private def parseDurationStep(x: String): DurationStep = x match {
    case StepHeader(name, params) => name match {
      case "warmup" => WarmupStep.tupled(expect(params))
      case "run" | "bike" => IntervalStep.tupled(expect(params))
      case "recover" => RecoverStep.tupled(expect(params))
      case "cooldown" => CooldownStep.tupled(expect(params))
      case _ => throw new IllegalArgumentException(s"Duration step type was expected, $name")
    }
    case _ => throw new IllegalArgumentException(s"Cannot parse step type $x")
  }

  private def expect(x: String): (Duration, Option[Target]) = x match {
    case ParamsRx(duration, _, target) =>
      val maybeTarget = Option(target).filter(_.trim.nonEmpty).map(Target.parse)
      (Duration.parse(duration.trim), maybeTarget)
    case _ => throw new IllegalArgumentException(s"Cannot parse step parameters $x")
  }
}

