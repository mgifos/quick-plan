package com.github.mgifos.workouts.model

import scala.io.Source
import scala.util.Using

import com.github.tototoshi.csv.CSVReader

class WeeklyPlan(csv: Array[Byte])(using msys: MeasurementSystem) {

  type WeekRow = List[String]

  private lazy val processed: Seq[Option[Workout]] = {

    def weekPlan(week: WeekRow, previousWeeks: Seq[Option[Workout]]): Seq[Option[Workout]] =
      Seq
        .tabulate(7) { weekDayNo =>
          week.lift(weekDayNo + 1).flatMap(text => Option(text.trim).filter(_.nonEmpty))
        }
        .foldLeft(Seq.empty[Option[Workout]])((acc, maybeDayText) =>
          acc :+ maybeDayText.map { dayText =>
            Workout.parse(dayText) match {
              case note: WorkoutNote =>
                onlyDefs(previousWeeks ++ acc).find(_.name == dayText).map(_.toRef).getOrElse(note)
              case w: Workout => w
            }
          }
        )

    def loop(weeks: List[WeekRow], acc: Seq[Option[Workout]]): Seq[Option[Workout]] = weeks match {
      case Nil => acc
      case week :: rest => loop(rest, acc ++ weekPlan(week, acc))
    }

    val rows = Using.resource(CSVReader.open(Source.fromBytes(csv)))(_.all())
    loop(rows.filter(isAValidWeek), Seq())
  }

  /**
   * @return
   *   all workout definitions defined in this plan
   */
  def workouts: Seq[WorkoutDef] = onlyDefs(processed)

  /**
   * @return
   *   optional workout refs (defs included as refs)
   */
  def get(): Seq[Option[WorkoutRef]] = processed.map {
    case Some(x: WorkoutDef) => Some(x.toRef)
    case Some(x: WorkoutRef) => Some(x)
    case _ => None
  }

  def invalid(): Seq[Workout] = processed.collect {
    case Some(x) if !x.valid => x
  }

  private def isAValidWeek(w: Seq[String]) =
    w.headOption.exists(no => no.trim.nonEmpty && no.forall(_.isDigit))

  private def onlyDefs(days: Seq[Option[Workout]]) = days.flatMap {
    case Some(wdef: WorkoutDef) => Some(wdef)
    case _ => None
  }
}
