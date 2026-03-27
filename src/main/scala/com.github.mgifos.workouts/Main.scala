package com.github.mgifos.workouts

import java.io.{ BufferedReader, InputStreamReader }
import java.nio.file.{ Files, Paths }
import java.time.LocalDate

import cats.effect.{ ExitCode, IO, IOApp }
import com.github.mgifos.workouts.model._
import com.typesafe.scalalogging.Logger
import org.http4s.ember.client.EmberClientBuilder
import scopt.OParser

enum Mode:
  case `import`, schedule

case class Config(
  mode: Option[Mode] = None,
  system: MeasurementSystem = MeasurementSystem.metric,
  csv: String = "",
  delete: Boolean = false,
  autoCooldown: Boolean = false,
  email: String = "",
  password: String = "",
  start: LocalDate = LocalDate.MIN,
  end: LocalDate = LocalDate.MIN
)

object Main extends IOApp {

  private val log = Logger(getClass)

  given measurementSystemRead: scopt.Read[MeasurementSystem] =
    scopt.Read.reads(MeasurementSystem.named)

  private val builder = OParser.builder[Config]

  val parser: OParser[Unit, Config] = {
    import builder._
    OParser.sequence(
      programName("quick-plan"),
      head("\nquick-plan", "0.x\n"),
      opt[String]('e', "email")
        .action((x, c) => c.copy(email = x))
        .text("E-mail to login to Garmin Connect"),
      opt[String]('p', "password")
        .action((x, c) => c.copy(password = x))
        .text("Password to login to Garmin Connect"),
      opt[MeasurementSystem]('m', "measurement_system")
        .action((x, c) => c.copy(system = x))
        .text(
          """"metric" (default) or "imperial" (miles, inches, ...) measurement system choice."""
        ),
      opt[Unit]('x', "delete")
        .action((_, c) => c.copy(delete = true))
        .text(
          "Delete all existing workouts with same names as the ones contained within the file. In case of import/schedule commands, " +
            "this will be done before the actual action."
        ),
      opt[Unit]('c', "auto-cooldown")
        .action((_, c) => c.copy(autoCooldown = true))
        .text(
          "Add automatically cooldown: lap-button as an additional last step of each workout definition."
        ),
      help("help").text("prints this usage text"),
      note(""),
      arg[String]("<file>")
        .required()
        .action((x, c) => c.copy(csv = x))
        .text("File with a weekly based plan in CSV format"),
      note("\n"),
      cmd("import")
        .action((_, c) => c.copy(mode = Some(Mode.`import`)))
        .text(
          "Imports all workout definitions from CSV file. If it's omitted, it is will be on by default."
        ),
      note(""),
      cmd("schedule")
        .action((_, c) => c.copy(mode = Some(Mode.schedule)))
        .text(
          "Schedules your weekly plan defined in CSV in Garmin Connect calendar, starting from the first day of first week or" +
            " ending on the last day of the last week. Either start or end date must be entered so the scheduling can be done" +
            " properly. In case both are entered, start date has priority. All dates have to be entered in ISO date format" +
            " e.g. '2018-03-24'.\n"
        )
        .children(
          opt[String]('s', "start")
            .action((x, c) => c.copy(start = LocalDate.parse(x)))
            .text("Date of the first day of the first week of the plan"),
          opt[String]('n', "end")
            .action((x, c) => c.copy(end = LocalDate.parse(x)))
            .text("Date of the last day of the last week of the plan\n"),
          checkConfig(c =>
            if (
              c.mode.contains(Mode.schedule) && c.start.isEqual(LocalDate.MIN) && c.end
                .isEqual(LocalDate.MIN)
            )
              failure("Either start or end date must be entered!")
            else success
          )
        ),
      note("EXAMPLES").text(
        "EXAMPLES\n\n" +
          "#Imports all the workouts from ultra 80k plan\nquick-plan import -e your-mail-address@example.com ultra-80k-runnersworld.csv" +
          "\n\n#Deletes all the workouts from ultra 80k plan\nquick-plan -x your-mail-address@example.com ultra-80k-runnersworld.csv" +
          "\n\n#Schedules ultra 80k plan targeting 28-4-2018 for a race day, while previously deleting if any with the same name already exists\nquick-plan schedule -n 2018-04-29 -x -e your-mail-address@example.com ultra-80k-runnersworld.csv"
      ),
      note("")
    )
  }

  def run(args: List[String]): IO[ExitCode] =
    OParser.parse(parser, args, Config()) match {
      case Some(config) =>
        execute(config).as(ExitCode.Success).handleError { ex =>
          log.error(s"Error: ${ex.getMessage}")
          ExitCode.Error
        }
      case None => IO.pure(ExitCode.Error)
    }

  private def readLine(prompt: String): IO[String] = IO.blocking {
    print(prompt)
    Option(System.console()) match {
      case Some(c) => c.readLine()
      case None => new BufferedReader(new InputStreamReader(System.in)).readLine()
    }
  }

  private def readPassword(prompt: String): IO[String] = IO.blocking {
    print(prompt)
    Option(System.console()) match {
      case Some(c) => new String(c.readPassword())
      case None => new BufferedReader(new InputStreamReader(System.in)).readLine()
    }
  }

  private def execute(config: Config): IO[Unit] =
    for {
      plan <- IO.blocking(
        new WeeklyPlan(Files.readAllBytes(Paths.get(config.csv)))(using config.system)
      )
      _ <-
        if (plan.invalid().isEmpty) proceedToGarmin(config, plan)
        else {
          plan.invalid().foreach(i => log.warn(i.toString))
          println("Your plan contains some invalid items.")
          print("Do you want to proceed to Garmin by skipping these items? [Y/n]")
          readLine("").flatMap {
            case null | "" | "y" | "Y" => proceedToGarmin(config, plan)
            case _ => IO.unit
          }
        }
    } yield ()

  private def proceedToGarmin(config: Config, plan: WeeklyPlan): IO[Unit] =
    for {
      email <-
        if (config.email.nonEmpty) IO.pure(config.email)
        else readLine("Please enter your email address to login to Garmin Connect: ")
      password <-
        if (config.password.nonEmpty) IO.pure(config.password)
        else readPassword("Password: ")
      _ <- EmberClientBuilder.default[IO].build.use { client =>
        GarminConnect.make(email, password, client).flatMap { garmin =>
          syncGarmin(config, plan, garmin)
        }
      }
    } yield ()

  private def syncGarmin(config: Config, plan: WeeklyPlan, garmin: GarminConnect): IO[Unit] = {

    def deleteWorkoutsTask(
        workouts: List[String]
    )(using session: GarminSession): IO[Option[String]] =
      if (config.delete) garmin.deleteWorkouts(workouts).map(c => Some(s"$c deleted"))
      else IO.pure(None)

    def createWorkoutsTask(
        workouts: List[WorkoutDef]
    )(using session: GarminSession): IO[Option[List[GarminWorkout]]] =
      if (config.mode.exists(List(Mode.`import`, Mode.schedule).contains(_)))
        garmin.createWorkouts(workouts).map(Some(_))
      else IO.pure(None)

    def scheduleTask(
        workouts: List[GarminWorkout]
    )(using session: GarminSession): IO[Option[String]] =
      if (config.mode.contains(Mode.schedule)) {
        val start = (config.start, config.end) match {
          case (_, end) if !end.isEqual(LocalDate.MIN) => end.minusDays(plan.get().length - 1)
          case (from, _) => from
        }
        val woMap: Map[String, GarminWorkout] = workouts.map(ga => ga.name -> ga).toMap
        val spec = plan
          .get()
          .zipWithIndex
          .collect {
            case (Some(ref), day) if !start.plusDays(day).isBefore(LocalDate.now()) =>
              start.plusDays(day) -> woMap(ref.name)
          }
          .toList
        garmin.schedule(spec).map(c => Some(s"$c scheduled"))
      } else IO.pure(None)

    def transformWorkouts(workouts: List[WorkoutDef]): List[WorkoutDef] =
      if (config.autoCooldown)
        workouts.map(w => w.copy(steps = w.steps :+ CooldownStep(LapButtonPressed)))
      else workouts

    val workouts = transformWorkouts(plan.workouts.toList)

    garmin.login().flatMap {
      case Right(s) =>
        given session: GarminSession = s
        for {
          maybeDeleteMessage <- deleteWorkoutsTask(workouts.map(_.name))
          maybeGarminWorkouts <- createWorkoutsTask(workouts)
          maybeScheduleMessage <- scheduleTask(
            maybeGarminWorkouts.fold(List.empty[GarminWorkout])(identity)
          )
        } yield {
          log.info("\nStatistics:")
          maybeDeleteMessage.foreach(msg => log.info("  " + msg))
          maybeGarminWorkouts.foreach(ws => log.info(s"  ${ws.length} imported"))
          maybeScheduleMessage.foreach(msg => log.info("  " + msg))
          log.info("Logging out and closing connection...")
          log.info("Bye")
        }
      case Left(loginFailureMessage) =>
        IO(log.error(loginFailureMessage))
    }
  }
}
