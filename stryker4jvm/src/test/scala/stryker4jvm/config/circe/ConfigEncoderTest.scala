package stryker4jvm.config.circe

import fs2.io.file.Path
import io.circe.Json.*
import io.circe.syntax.*
import org.scalactic.source.Position
import stryker4s.config.*
import stryker4s.testutil.Stryker4sSuite

class ConfigEncoderTest extends Stryker4jvmSuite {
  val workspaceLocation = Path("workspace").absolute.toString
  describe("configEncoder") {
    // Config updated such that adding more language mutator configs does not require changing this test
    it("should be able to encode a minimal config") {
      expectJsonConfig(
        defaultConfig.copy(mutatorConfigs = Map(".scala" -> new LanguageMutatorConfig("2_13", new util.HashSet()))),
        defaultConfigJson,
        s"""{"mutate":[],"test-filter":[],"base-dir":"${workspaceLocation.replace(
            "\\",
            "\\\\"
          )}","reporters":["console","html"],"files":[],"thresholds":{"high":80,"low":60,"break":0},"dashboard":{"base-url":"https://dashboard.stryker-mutator.io","report-type":"full"},"timeout":5000,"timeout-factor":1.5,"legacy-test-runner":false,"debug":{"log-test-runner-stdout":false,"debug-test-runner":false},"mutator-configs":{".scala":{"dialect":"2_13","excluded-mutations":[]}}}"""
      )
    }

    it("should be able to encode a filled config") {
      expectJsonConfig(
        defaultConfig.copy(
          mutate = Seq("**/main/scala/**.scala"),
          testFilter = Seq("foo.scala"),
          files = Seq("file.scala"),
          maxTestRunnerReuse = Some(2),
          dashboard = DashboardOptions(
            project = Some("myProject"),
            version = Some("1.3.3.7"),
            module = Some("myModule")
          ),
          debug = DebugOptions(
            logTestRunnerStdout = true,
            debugTestRunner = true
          ),
          mutatorConfigs = Map(".scala" -> new LanguageMutatorConfig("2_13", new util.HashSet()))
        ),
        defaultConfigJson.mapObject(
          _.add("mutate", arr(fromString("**/main/scala/**.scala")))
            .add("test-filter", arr(fromString("foo.scala")))
            .add("files", arr(fromString("file.scala")))
            .add("max-test-runner-reuse", fromInt(2))
            .add(
              "dashboard",
              obj(
                "base-url" -> fromString(defaultConfig.dashboard.baseUrl.toString()),
                "report-type" -> fromString("full"),
                "project" -> fromString("myProject"),
                "version" -> fromString("1.3.3.7"),
                "module" -> fromString("myModule")
              )
            )
            .add(
              "debug",
              obj(
                "log-test-runner-stdout" -> fromBoolean(true),
                "debug-test-runner" -> fromBoolean(true)
              )
            )
        ),
        s"""{"mutate":["**/main/scala/**.scala"],"test-filter":["foo.scala"],"base-dir":"${workspaceLocation.replace(
            "\\",
            "\\\\"
          )}","reporters":["console","html"],"files":["file.scala"],"thresholds":{"high":80,"low":60,"break":0},"dashboard":{"base-url":"https://dashboard.stryker-mutator.io","report-type":"full","project":"myProject","version":"1.3.3.7","module":"myModule"},"timeout":5000,"timeout-factor":1.5,"max-test-runner-reuse":2,"legacy-test-runner":false,"debug":{"log-test-runner-stdout":true,"debug-test-runner":true},"mutator-configs":{".scala":{"dialect":"2_13","excluded-mutations":[]}}}"""
      )
    }
  }

  def expectJsonConfig(config: Config, json: io.circe.Json, jsonString: String)(implicit pos: Position) = {
    val result = config.asJson

    result.noSpaces shouldBe jsonString
    result shouldBe json
  }

  def defaultConfig: Config = Config.default.copy(baseDir = Path("workspace"))

  def defaultConfigJson = obj(
    "mutate" -> arr(),
    "files" -> arr(),
    "test-filter" -> arr(),
    "base-dir" -> fromString(workspaceLocation),
    "reporters" -> arr(fromString("console"), fromString("html")),
    "thresholds" -> obj(
      "high" -> fromInt(80),
      "low" -> fromInt(60),
      "break" -> fromInt(0)
    ),
    "dashboard" -> obj(
      "base-url" -> fromString(defaultConfig.dashboard.baseUrl.toString()),
      "report-type" -> fromString("full")
    ),
    "timeout" -> fromInt(5000),
    "timeout-factor" -> fromDouble(1.5).get,
    "legacy-test-runner" -> False,
    "debug" -> obj(
      "log-test-runner-stdout" -> False,
      "debug-test-runner" -> False
    ),
    "mutator-configs" -> obj(
      ".scala" -> obj(
        "dialect" -> fromString("2_13"),
        "excluded-mutations" -> arr()
      )
    )
  )
}
