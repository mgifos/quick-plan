package com.github.mgifos.workouts

import java.nio.file.{Files, Paths}
import java.time.LocalDate

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.github.mgifos.workouts.model.{WeeklyPlan, _}
import com.typesafe.scalalogging.Logger
import scopt.OptionParser

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

object Modes extends Enumeration {
  type Mode = Value
  val `import`: Mode = Value("import")
  val schedule: Mode = Value("schedule")
}

case class Config(mode: Option[Modes.Mode] = None,
                  system: MeasurementSystems.MeasurementSystem = MeasurementSystems.metric,
                  csv: String = "",
                  delete: Boolean = false,
                  autoCooldown: Boolean = false,
                  email: String = "",
                  password: String = "",
                  start: LocalDate = LocalDate.MIN,
                  end: LocalDate = LocalDate.MIN)

object Main extends App {

  implicit val system: ActorSystem = ActorSystem("quick-plan")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  implicit val mSystemRead: scopt.Read[MeasurementSystems.MeasurementSystem] = scopt.Read.reads(MeasurementSystems.named)

  val log = Logger(getClass)

  parser.parse(args, Config()) match {

    case Some(config) =>
      val worker = run(config).andThen {
        case _ => shutdown()
      }
      Await.result(worker, 10.minutes)
      log.info("Bye")

    case None => shutdown()
  }

  def parser = new OptionParser[Config]("quick-plan") {

    head("\nquick-plan", "0.x\n")

    opt[String]('e', "email").action((x, c) => c.copy(email = x)).text("E-mail to login to Garmin Connect")

    opt[String]('p', "password").action((x, c) => c.copy(password = x)).text("Password to login to Garmin Connect")

    opt[MeasurementSystems.MeasurementSystem]('m', "measurement_system")
      .action((x, c) => c.copy(system = x))
      .text(""""metric" (default) or "imperial" (miles, inches, ...) measurement system choice.""")

    opt[Unit]('x', "delete")
      .action((_, c) => c.copy(delete = true))
      .text("Delete all existing workouts with same names as the ones contained within the file. In case of import/schedule commands, " +
        "this will be done before the actual action.")

    opt[Unit]('c', "auto-cooldown")
      .action((_, c) => c.copy(autoCooldown = true))
      .text("Add automatically cooldown: lap-button as an additional last step of each workout definition.")

    help("help").text("prints this usage text")

    note("")

    arg[String]("<file>").required().action((x, c) => c.copy(csv = x)).text("File with a weekly based plan in CSV format")

    note("\n")

    cmd("import")
      .action((_, c) => c.copy(mode = Some(Modes.`import`)))
      .text("Imports all workout definitions from CSV file. If it's omitted, it is will be on by default.")

    note("")

    cmd("schedule")
      .action((_, c) => c.copy(mode = Some(Modes.schedule)))
      .text(
        "Schedules your weekly plan defined in CSV in Garmin Connect calendar, starting from the first day of first week or" +
          " ending on the last day of the last week. Either start or end date must be entered so the scheduling can be done" +
          " properly. In case both are entered, start date has priority. All dates have to be entered in ISO date format" +
          " e.g. '2018-03-24'.\n")
      .children(
        opt[String]('s', "start").action((x, c) => c.copy(start = LocalDate.parse(x))).text("Date of the first day of the first week of the plan"),
        opt[String]('n', "end").action((x, c) => c.copy(end = LocalDate.parse(x))).text("Date of the last day of the last week of the plan\n"),
        checkConfig(
          c =>
            if (c.mode.contains(Modes.schedule) && c.start.isEqual(LocalDate.MIN) && c.end.isEqual(LocalDate.MIN))
              failure("Either start or end date must be entered!")
            else success)
      )

    note("EXAMPLES").text(
      "EXAMPLES\n\n" +
        "#Imports all the workouts from ultra 80k plan\nquick-plan import -e your-mail-address@example.com ultra-80k-runnersworld.csv" +
        "\n\n#Deletes all the workouts from ultra 80k plan\nquick-plan -x your-mail-address@example.com ultra-80k-runnersworld.csv" +
        "\n\n#Schedules ultra 80k plan targeting 28-4-2018 for a race day, while previously deleting if any with the same name already exists\nquick-plan schedule -n 2018-04-29 -x -e your-mail-address@example.com ultra-80k-runnersworld.csv")

    note("")

    override def showUsageOnError = true
  }

  def run(implicit config: Config): Future[Unit] = {

    implicit val plan: WeeklyPlan = new WeeklyPlan(Files.readAllBytes(Paths.get(config.csv)))(config.system)

    val console = System.console()

    def proceedToGarmin() = {
      val email =
        if (config.email.nonEmpty) config.email
        else {
          print("Please enter your email address to login to Garmin Connect: ")
          console.readLine()
        }
      val password =
        if (config.password.nonEmpty) config.password
        else {
          print("Password: ")
          new String(console.readPassword())
        }

      syncGarmin(config, plan, new GarminConnect(email, password))
    }

    if (plan.invalid().isEmpty) proceedToGarmin()
    else {
      plan.invalid().foreach(i => log.warn(i.toString))
      println("Your plan contains some invalid items.")
      print("Do you want to proceed to Garmin by skipping these items? [Y/n]")
      "" + console.readLine() match {
        case "" | "y" | "Y" => proceedToGarmin()
        case _              => Future.successful(())
      }
    }
  }

  private def syncGarmin(implicit config: Config, plan: WeeklyPlan, garmin: GarminConnect): Future[Unit] = {

    def deleteWorkoutsTask(workouts: Seq[String])(implicit config: Config, garmin: GarminConnect, session: GarminSession): Future[Option[String]] = {
      if (config.delete)
        garmin.deleteWorkouts(workouts).map(c => Some(s"$c deleted"))
      else
        Future.successful(None)
    }

    def createWorkoutsTask(
        workouts: Seq[WorkoutDef])(implicit config: Config, garmin: GarminConnect, session: GarminSession): Future[Option[Seq[GarminWorkout]]] = {
      if (config.mode.exists(Seq(Modes.`import`, Modes.schedule).contains))
        garmin.createWorkouts(workouts).map(Option.apply)
      else
        Future.successful(None)
    }

    def scheduleTask(workouts: Seq[GarminWorkout])(implicit
                                                   config: Config,
                                                   garmin: GarminConnect,
                                                   plan: WeeklyPlan,
                                                   session: GarminSession): Future[Option[String]] = {

      if (config.mode.contains(Modes.schedule)) {

        val start = (config.start, config.end) match {
          case (_, end) if !end.isEqual(LocalDate.MIN) => end.minusDays(plan.get().length - 1)
          case (from, _)                               => from
        }

        val woMap: Map[String, GarminWorkout] = Map(workouts.map(ga => ga.name -> ga): _*)
        val spec = plan
          .get()
          .zipWithIndex
          .collect {
            case (Some(ref), day) if !start.plusDays(day).isBefore(LocalDate.now()) => start.plusDays(day) -> woMap(ref.name)
          }
          .to[Seq]
        garmin.schedule(spec).map(c => Some(s"$c scheduled"))
      } else
        Future.successful(None)
    }

    def transformWorkouts(workouts: Seq[WorkoutDef]): Seq[WorkoutDef] = {
      if (config.autoCooldown) workouts.map(w => w.copy(steps = w.steps :+ CooldownStep(LapButtonPressed)))
      else workouts
    }

    val workouts = transformWorkouts(plan.workouts.toIndexedSeq)

    garmin.login().flatMap {
      case Right(s) =>
        implicit val session: GarminSession = s
        for {
          maybeDeleteMessage <- deleteWorkoutsTask(workouts.map(_.name))
          maybeGarminWorkouts <- createWorkoutsTask(workouts)
          maybeScheduleMessage <- scheduleTask(maybeGarminWorkouts.fold(Seq.empty[GarminWorkout])(identity))
        } yield {
          log.info("\nStatistics:")
          maybeDeleteMessage.foreach(msg => log.info("  " + msg))
          maybeGarminWorkouts.foreach(workouts => log.info(s"  ${workouts.length} imported"))
          maybeScheduleMessage.foreach(msg => log.info("  " + msg))
          log.info("Logging out and closing connection...")
        }
      case Left(loginFailureMessage) =>
        log.error(loginFailureMessage)
        Future.successful(())
    }
  }

  private def shutdown() = Await.result(Http().shutdownAllConnectionPools().flatMap(_ => system.terminate()), 10.minutes)

}
