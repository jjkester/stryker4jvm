package stryker4jvm.reporting

import cats.effect.IO
import fs2.Pipe
import cats.syntax.applicative.*
import mutationtesting.{MetricsResult, MutationTestResult}
import stryker4jvm.config.Config

import java.nio.file.Path
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

class IOReporter {
  def mutantTested: Pipe[IO, MutantTestedEvent, Nothing] = in => in.drain
  def onRunFinished(runReport: FinishedRunEvent): IO[Unit] = runReport.pure[IO].void
}

final case class MutantTestedEvent(totalMutants: Int) extends AnyVal

final case class FinishedRunEvent(
    report: MutationTestResult[Config],
    metrics: MetricsResult,
    duration: FiniteDuration,
    reportsLocation: Path
)
