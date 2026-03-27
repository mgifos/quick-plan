package com.github.mgifos.workouts

import java.nio.file.Files

import cats.effect.ExitCode
import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scopt.OParser

import com.github.mgifos.workouts.model.MeasurementSystem

class MainSpec extends AnyWordSpec with Matchers {

  // Write a minimal valid CSV plan to a temp file for tests that reach execute()
  private lazy val tempCsv: String = {
    val f = Files.createTempFile("quick-plan-test", ".csv")
    Files.writeString(
      f,
      "1,\"running: r1\n- run: 1km\n- cooldown: lap-button\",,,,,,,\n"
    )
    f.toAbsolutePath.toString
  }

  // -------------------------------------------------------------------------
  // Parser tests (pure — no IO)
  // -------------------------------------------------------------------------

  "Main.parser" should {

    "accept a bare file argument" in {
      OParser.parse(Main.parser, List("plan.csv"), Config()) shouldBe
        Some(Config(csv = "plan.csv"))
    }

    "accept the import sub-command" in {
      val result = OParser.parse(Main.parser, List("import", "plan.csv"), Config())
      result.map(_.mode) shouldBe Some(Some(Mode.`import`))
      result.map(_.csv) shouldBe Some("plan.csv")
    }

    "accept the schedule sub-command with a start date" in {
      val result =
        OParser.parse(Main.parser, List("schedule", "-s", "2025-03-01", "plan.csv"), Config())
      result.map(_.mode) shouldBe Some(Some(Mode.schedule))
      result.flatMap(_.start).map(_.toString) shouldBe Some("2025-03-01")
    }

    "accept the schedule sub-command with an end date" in {
      val result =
        OParser.parse(Main.parser, List("schedule", "-n", "2025-03-28", "plan.csv"), Config())
      result.flatMap(_.end).map(_.toString) shouldBe Some("2025-03-28")
    }

    "accept -e / -p / -m flags" in {
      val result = OParser.parse(
        Main.parser,
        List("-e", "u@test.com", "-p", "secret", "-m", "imperial", "plan.csv"),
        Config()
      )
      result.map(_.email) shouldBe Some("u@test.com")
      result.map(_.password) shouldBe Some("secret")
      result.map(_.system) shouldBe Some(MeasurementSystem.imperial)
    }

    "accept the -x delete flag" in {
      OParser.parse(Main.parser, List("-x", "plan.csv"), Config()).map(_.delete) shouldBe Some(true)
    }

    "accept the -c auto-cooldown flag" in {
      OParser.parse(Main.parser, List("-c", "plan.csv"), Config()).map(_.autoCooldown) shouldBe
        Some(true)
    }

    "reject schedule without a date" in {
      OParser.parse(Main.parser, List("schedule", "plan.csv"), Config()) shouldBe None
    }

    "return None when required <file> argument is missing" in {
      OParser.parse(Main.parser, List("import"), Config()) shouldBe None
    }
  }

  // -------------------------------------------------------------------------
  // run() tests (IO)
  // -------------------------------------------------------------------------

  "Main.run" should {

    "return ExitCode.Error when no arguments are provided" in {
      Main.run(List.empty).unsafeRunSync() shouldBe ExitCode.Error
    }

    "return ExitCode.Error when schedule is missing a date" in {
      Main.run(List("plan.csv", "schedule")).unsafeRunSync() shouldBe ExitCode.Error
    }

    "return ExitCode.Error when the CSV file does not exist" in {
      Main
        .run(List("nonexistent-file.csv", "import", "-e", "u@test.com", "-p", "pw"))
        .unsafeRunSync() shouldBe ExitCode.Error
    }

    "return ExitCode.Error when Garmin login fails (network unavailable)" in {
      // Provides a real CSV but the Garmin HTTP requests will fail → handleError → Error
      Main
        .run(List(tempCsv, "import", "-e", "u@test.com", "-p", "pw"))
        .unsafeRunSync() shouldBe ExitCode.Error
    }
  }
}
