package stryker4jvm.run.process

import stryker4jvm.scalatest.LogMatchers
import stryker4jvm.testutil.Stryker4jvmIOSuite

import scala.util.Properties

class ProcessRunnerTest extends Stryker4sIOSuite with LogMatchers {
  describe("resolveRunner") {
    it("should resolve the proper runner for the current OS") {
      val result = ProcessRunner()

      if (Properties.isWin)
        result shouldBe a[WindowsProcessRunner]
      else
        result shouldBe a[UnixProcessRunner]
    }
  }
}
