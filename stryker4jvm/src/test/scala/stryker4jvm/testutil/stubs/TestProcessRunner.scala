package stryker4jvm.testutil.stubs

import cats.effect.IO
import fs2.io.file.Path
import stryker4jvm.config.Config
import stryker4jvm.core.logging.Logger
import stryker4jvm.logging.FansiLogger
import stryker4jvm.run.process.{Command, ProcessRunner}

import scala.util.{Success, Try}

object TestProcessRunner {
  def apply(testRunExitCode: Try[Int]*)(implicit log: FansiLogger): TestProcessRunner =
    new TestProcessRunner(true, testRunExitCode*)
  def failInitialTestRun()(implicit log: FansiLogger): TestProcessRunner = new TestProcessRunner(false)
}

class TestProcessRunner(initialTestRunSuccess: Boolean, testRunExitCode: Try[Int]*)(implicit log: FansiLogger)
    extends ProcessRunner {
  val timesCalled: Iterator[Int] = Iterator.from(0)

  /** Keep track on the amount of times the function is called.
    *
    * Also return an exit code which the test runner would do as well.
    */
  override def apply(command: Command, workingDir: Path, envVar: (String, String)*)(implicit
      config: Config
  ): IO[Try[Int]] = {
    if (envVar.isEmpty) {
      IO.pure(Success(if (initialTestRunSuccess) 0 else 1))
    } else {
      timesCalled.next()
      IO.pure(testRunExitCode(envVar.map(_._2).head.toInt))
    }
  }
}
