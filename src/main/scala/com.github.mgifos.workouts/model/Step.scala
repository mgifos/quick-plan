package com.github.mgifos.workouts.model

sealed trait Step {
  def `type`: String
}

abstract class DurationStep(stepType: String, duration: Duration, target: Option[Target] = None) extends Step {
  override def `type` = stepType
}

case class WarmupStep(duration: Duration, target: Option[Target] = None) extends DurationStep("warmup", duration)

case class RunStep(duration: Duration, target: Option[Target] = None) extends DurationStep("run", duration)

case class RecoverStep(duration: Duration, target: Option[Target] = None) extends DurationStep("recover", duration)

case class CooldownStep(duration: Duration, target: Option[Target] = None) extends DurationStep("cooldown", duration)

case class RepeatStep(count: Int, definition: Seq[DurationStep]) extends Step {
  override def `type` = "repeat"
}

object Step {

  private val StepRx = """^(-\s\w*:\s.*)((\n\s{1,}-\s.*)*)$""".r
  private val StepHeader = """^\s*-\s*(\w*):(.*)$""".r
  private val ParamsRx = """^([\w\.:\s]+)\s*(@(.*))?$""".r

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
      case "run" => RunStep.tupled(expect(params))
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

