package com.github.mgifos.workouts

import java.nio.file.{ Files, Paths }
import java.time.LocalDate

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.github.mgifos.workouts.model.WeeklyPlan
import com.typesafe.scalalogging.Logger
import scopt.OptionParser

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContextExecutor, Future }

object Modes extends Enumeration {
  type Mode = Value
  val `import` = Value("import")
  val schedule = Value("schedule")
}

case class Config(
  mode: Modes.Mode = Modes.`import`,
  csv: String = "",
  delete: Boolean = false,
  email: String = "",
  password: String = "",
  start: LocalDate = LocalDate.MIN,
  end: LocalDate = LocalDate.MIN)

object Main extends App {

  implicit val system: ActorSystem = ActorSystem("quick-plan")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  val log = Logger(getClass)

  parser.parse(args, Config()) match {

    case Some(config) =>
      val worker = run(config).andThen {
        case _ =>
          shutdown()
          log.info("Logged out and closed connection")
      }
      Await.result(worker, 10.minutes)
      log.info("Bye")

    case None => shutdown()
  }

  def parser = new OptionParser[Config]("quick-plan") {

    head("\nquick-plan", "0.x\n")

    opt[String]('e', "email").action((x, c) => c.copy(email = x)).text("E-mail to login to Garmin Connect")

    opt[String]('p', "password").action((x, c) => c.copy(password = x)).text("Password to login to Garmin Connect")

    opt[Unit]('x', "delete").action((_, c) => c.copy(delete = true)).text("Delete all existing workouts with same names as the ones that are going to be imported.")

    help("help").text("prints this usage text")

    note("")

    arg[String]("<file>").required().action((x, c) => c.copy(csv = x)).text("File with a weekly based plan in CSV format")

    note("\n")

    cmd("import").
      action((_, c) => c.copy(mode = Modes.`import`)).text(
        "Imports all workout definitions from CSV file. If it's omitted, it is will be on by default.")

    note("")

    cmd("schedule").action((_, c) => c.copy(mode = Modes.schedule)).text(
      "Schedules your weekly plan defined in CSV in Garmin Connect calendar, starting from the first day of first week or" +
        " ending on the last day of the last week. Either start or end date must be entered so the scheduling can be done" +
        " properly. In case both are entered, start date has priority. All dates have to be entered in ISO date format" +
        " e.g. '2018-03-24'.\n")
      .children(
        opt[String]('s', "start").action((x, c) => c.copy(start = LocalDate.parse(x))).text("Date of the first day of the first week of the plan"),
        opt[String]('n', "end").action((x, c) => c.copy(end = LocalDate.parse(x))).text("Date of the last day of the last week of the plan\n"),
        checkConfig(c =>
          if (c.mode == Modes.schedule && c.start.isEqual(LocalDate.MIN) && c.end.isEqual(LocalDate.MIN))
            failure("Either start or end date must be entered!")
          else success))

    note("EXAMPLES").text("EXAMPLES\n\nSchedules ultra 80k plan targeting 28-4-2018 for a race day (also deletes existing workouts with the same names)" +
      "\n\nquick-plan schedule -n 2018-04-29 -x -e your-mail-address@example.com ultra-80k-runnersworld.csv")

    note("")

    override def showUsageOnError = true
  }

  def run(implicit config: Config): Future[Unit] = {

    val console = System.console()

    val email = if (config.email.nonEmpty) config.email else {
      print("Please enter your email address to login to Garmin Connect: ")
      console.readLine()
    }

    val password = if (config.password.nonEmpty) config.password else {
      print("Password: ")
      new String(console.readPassword())
    }

    implicit val plan: WeeklyPlan = new WeeklyPlan(Files.readAllBytes(Paths.get(config.csv)))

    implicit val garmin: GarminConnect = new GarminConnect(email, password)

    val workouts = plan.workouts.toIndexedSeq

    for {
      maybeDeleteMessage <- deleteWorkoutsTask(workouts.map(_.name))
      garminWorkouts <- garmin.createWorkouts(workouts)
      maybeScheduleMessage <- scheduleTask(garminWorkouts)
    } yield {
      log.info("\nStatistics:")
      maybeDeleteMessage.foreach(msg => log.info("  " + msg))
      log.info(s"  ${garminWorkouts.length} imported")
      maybeScheduleMessage.foreach(msg => log.info("  " + msg))
    }
  }

  /**
   * Deletes existing workouts with the same names or not
   */
  private def deleteWorkoutsTask(workouts: Seq[String])(implicit config: Config, garmin: GarminConnect): Future[Option[String]] = {
    if (config.delete)
      garmin.deleteWorkouts(workouts).map(c => Some(s"$c deleted"))
    else
      Future.successful(None)
  }

  private def scheduleTask(workouts: Seq[GarminWorkout])(implicit config: Config, garmin: GarminConnect, plan: WeeklyPlan): Future[Option[String]] = {

    if (config.mode == Modes.schedule) {

      val start = (config.start, config.end) match {
        case (_, end) if !end.isEqual(LocalDate.MIN) => end.minusDays(plan.get.length - 1)
        case (from, _) => from
      }

      val woMap: Map[String, GarminWorkout] = Map(workouts.map(ga => ga.name -> ga): _*)
      val spec = plan.get.zipWithIndex.collect {
        case (Some(ref), day) if !start.plusDays(day).isBefore(LocalDate.now()) => start.plusDays(day) -> woMap(ref.name)
      }.to[Seq]
      garmin.schedule(spec).map(c => Some(s"$c scheduled"))
    } else
      Future.successful(None)
  }

  private def shutdown() = Await.result(Http().shutdownAllConnectionPools().flatMap(_ => system.terminate()), 10.minutes)

}
