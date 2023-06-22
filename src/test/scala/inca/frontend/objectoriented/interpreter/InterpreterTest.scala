package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.parser.Parser
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class InterpreterTest extends AnyFunSuite {
  val parser: Parser = new Parser {}
  val typechecker: Typechecker = new Typechecker {}

  def runProg(path: String, args: Seq[Any]): Value = {
    val code = FileUtil.readFile(path)
    val mod = Parser.parse(code)
    typechecker.typecheck(mod)
    val main = mod.classes.flatMap(_.methods.find(_.isMain)).head
    new Interpreter(mod).interp(main, args.map(ScalaValue))
  }

  test("Base") {
    val res1 = runProg("objectoriented/unittests/base/Base1.oinca", Seq())
    val res2 = runProg("objectoriented/unittests/base/Base2.oinca", Seq())
    val res3 = runProg("objectoriented/unittests/base/Base3.oinca", Seq())
    assertResult(43)(res1.asScala)
    assertResult(43)(res2.asScala)
    assertResult(43)(res3.asScala)
  }

  test("Fib") {
    val res = runProg("objectoriented/unittests/Fib.oinca", Seq(11))
    assertResult(89)(res.asScala)
  }

  test("Fac") {
    val res = runProg("objectoriented/unittests/Fact.oinca", Seq(5))
    assertResult(120)(res.asScala)
  }

  test("Equals") {
    val res = runProg("objectoriented/unittests/Equals.oinca", Seq())
    assertResult(true)(res.asScala)
  }

  test("InstanceOf") {
    val res = runProg("objectoriented/unittests/InstanceOf.oinca", Seq())
    assertResult(true)(res.asScala)
  }

  test("TypeCast") {
    val res = runProg("objectoriented/unittests/TypeCast.oinca", Seq())
    assertResult(true)(res.asScala)
  }

  test("TypeCast failure") {
    val caught = intercept[TypeCastException] {
      runProg("objectoriented/unittests/TypeCastFail.oinca", Seq())
    }
    caught.obj.asObject match {
      case Some((cls, _, _)) => assertResult("A")(cls)
      case None => assert(false)
    }
    assertResult("B")(caught.typ)
  }

  test("FieldAccess") {
    val res = runProg("objectoriented/unittests/field/FieldAccess.oinca", Seq(16, 8))
    assertResult(2)(res.asScala)
  }

  test("FieldAccessNested") {
    val res = runProg("objectoriented/unittests/field/FieldAccessNested.oinca", Seq())
    assertResult(3)(res.asScala)
  }

  test("FieldDeclare") {
    val res = runProg("objectoriented/unittests/field/FieldDeclare.oinca", Seq())
    assertResult(3)(res.asScala)
  }

  test("FieldInheritance") {
    val res = runProg("objectoriented/unittests/field/FieldInheritance.oinca", Seq())
    assertResult(10)(res.asScala)
  }
}
