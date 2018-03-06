package com.github.mgifos.workouts.model

import com.github.tototoshi.csv.CSVReader

import scala.io.Source

class WeeklyPlan(csv: Array[Byte]) {

  type Week = List[String]

  private lazy val processed: Seq[Option[Workout]] = {

    def weekPlan(week: Week, acc: Seq[Option[Workout]]): Seq[Option[Workout]] = Seq.tabulate(7) { weekDayNo =>
      val maybeDayText = week.lift(weekDayNo + 1).flatMap(text => Option(text.trim).filter(_.nonEmpty))
      maybeDayText.map { dayText =>
        Workout.parseDef(dayText) match {
          case Right(definition) => definition
          case Left(_) => onlyDefs(acc).find(_.name == dayText).map(_.toRef).getOrElse(WorkoutNote(dayText))
        }
      }
    }

    def loop(weeks: List[Week], acc: Seq[Option[Workout]]): Seq[Option[Workout]] = weeks match {
      case Nil => acc
      case week :: rest => loop(rest, acc ++ weekPlan(week, acc))
    }

    loop(CSVReader.open(Source.fromBytes(csv)).all.filter(isAValidWeek), Seq())
  }

  /**
   * @return all workout definitions defined in this plan
   */
  def workouts: Seq[WorkoutDef] = onlyDefs(processed)

  /**
   * @return optional workout refs & notes of this plan per day
   */
  def get: Seq[Option[Workout]] = processed.map {
    case Some(x: WorkoutDef) => Some(x.toRef)
    case x => x
  }

  private def isAValidWeek(w: Seq[String]) = w.headOption.exists(no => no.trim.nonEmpty && no.forall(_.isDigit))

  private def onlyDefs(days: Seq[Option[Workout]]) = days.flatMap {
    case Some(wdef: WorkoutDef) => Some(wdef)
    case _ => None
  }
}