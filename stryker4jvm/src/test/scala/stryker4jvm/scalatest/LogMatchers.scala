package stryker4jvm.scalatest

import fansi.Attr
import fansi.Color.*
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.{BeforeAndAfterEach, Suite}
import stryker4jvm.core.logging.LogLevel
import stryker4jvm.core.logging.LogLevel.{Debug, Error, Info, Warn}
import stryker4jvm.logging.FansiLogger
import stryker4jvm.testutil.TestLogger

trait LogMatchers extends BeforeAndAfterEach {
  // Will cause a compile error if LogMatchers is used outside of a ScalaTest Suite
  this: Suite =>

  /** Override to `true` if you want tests to print logs to the console for debugging purposes
    *
    * @example
    *   `override def printLogs = true`
    */
  def printLogs: Boolean = false

  val testLogger = new TestLogger(printLogs)
  implicit val log: FansiLogger = new FansiLogger(testLogger)

  override protected def afterEach(): Unit = {
    try super.afterEach()
    finally testLogger.clear()
  }

  def loggedAsDebug = new LogMatcherWithLevel(Debug)
  def loggedAsInfo = new LogMatcherWithLevel(Info)
  def loggedAsWarning = new LogMatcherWithLevel(Warn)
  def loggedAsError = new LogMatcherWithLevel(Error)

  private[scalatest] class LogMatcherWithLevel(expectedLogLevel: LogLevel) extends BeMatcher[String] {
    def apply(expectedLogMessage: String): MatchResult = {
      testLogger.findEvent(expectedLogMessage) match {
        case None =>
          testLogger.findEventPlainText(expectedLogMessage) match {
            case Some((_, message)) =>
              val msg =
                s"""Log message was logged with level $expectedLogLevel, but with different colors.
                   |${Red("Obtained:")}
                   |${Attr.Reset.escape}$message
                   |
                   |${Green("Expected:")}
                   |${Attr.Reset.escape}$expectedLogMessage
                   |
                   |""".stripMargin
              MatchResult(matches = false, msg, msg)
            case None =>
              MatchResult(
                matches = false,
                s"Log message '$expectedLogMessage' wasn't logged at any level.",
                s"Log message '$expectedLogMessage' was logged as $expectedLogLevel."
              )
          }
        case Some((level, _)) =>
          val sameLogLevel = level == expectedLogLevel

          MatchResult(
            sameLogLevel,
            s"Log message '$expectedLogMessage' was logged but not on correct log level, expected [$expectedLogLevel] actual [$level].",
            s"Log message '$expectedLogMessage' was logged as $expectedLogLevel."
          )
      }
    }

  }
}
