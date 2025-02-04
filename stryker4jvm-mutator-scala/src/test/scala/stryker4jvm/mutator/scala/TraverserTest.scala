package stryker4jvm.mutator.scala

import stryker4jvm.mutator.scala.extensions.TreeExtensions.FindExtension

import scala.meta.Lit
import scala.meta.quasiquotes.*

import stryker4jvm.mutator.scala.testutil.Stryker4jvmSuite

class TraverserTest extends Stryker4jvmSuite {

  implicit val log = new ScalaLogger();
  val traverser = new TraverserImpl()

  describe("canPlace") {
    it("can not place inside case guards") {
      val code = q"""x.bar(2) match {
        case 1 if x.foo() => 1
      }"""

      val caseGuard = code.find(q"x.foo()").value
      val result = traverser.canPlace(caseGuard)
      result shouldBe None
    }

    it("can place in case body") {
      val code = q"""x.bar(2) match {
        case 1 if x.foo() => 3
      }"""
      val body = code.find(Lit.Int(3)).value
      val result = traverser.canPlace(body).value
      result shouldBe body
    }

    it("can not place inside annotations") {
      val code = q"""
      @SuppressWarnings(Array("stryker4jvm.mutation.MethodExpression"))
      val x = foo()
        """

      val annotation = code.find(Lit.String("stryker4jvm.mutation.MethodExpression")).value
      val result = traverser.canPlace(annotation)
      result shouldBe None
    }
  }
}
