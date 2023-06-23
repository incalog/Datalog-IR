package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.parser.Parser
import inca.frontend.objectoriented.transformations.AddMissingDefinitions
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class InterpreterTest extends AnyFunSuite {
  val parser: Parser = new Parser {}
  val typechecker: Typechecker = new Typechecker {}

  def runProg(path: String, args: Seq[Any]): Any = {
    val code = FileUtil.readFile(path)
    var mod = Parser.parse(code)
    mod = AddMissingDefinitions.transformModule(mod)
    typechecker.typecheck(mod)

    val main = mod.classes.flatMap(_.methods.find(_.isMain)).head
    val res = new Interpreter(mod).interp(main, args.map(ScalaValue))

    res.asScala
  }

  test("Base") {
    val res1 = runProg("objectoriented/unittests/base/Base1.oinca", Seq())
    val res2 = runProg("objectoriented/unittests/base/Base2.oinca", Seq())
    val res3 = runProg("objectoriented/unittests/base/Base3.oinca", Seq())
    assertResult(43)(res1)
    assertResult(43)(res2)
    assertResult(43)(res3)
  }

  test("Fib") {
    val res = runProg("objectoriented/unittests/Fib.oinca", Seq(11))
    assertResult(89)(res)
  }

  test("Fac") {
    val res = runProg("objectoriented/unittests/Fact.oinca", Seq(5))
    assertResult(120)(res)
  }

  test("Equals") {
    val res = runProg("objectoriented/unittests/Equals.oinca", Seq())
    assertResult(true)(res)
  }

  test("InstanceOf") {
    val res = runProg("objectoriented/unittests/InstanceOf.oinca", Seq())
    assertResult(true)(res)
  }

  test("TypeCast") {
    val res = runProg("objectoriented/unittests/TypeCast.oinca", Seq())
    assertResult(true)(res)
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
    assertResult(2)(res)
  }

  test("FieldAccessNested") {
    val res = runProg("objectoriented/unittests/field/FieldAccessNested.oinca", Seq())
    assertResult(3)(res)
  }

  test("FieldDeclare") {
    val res = runProg("objectoriented/unittests/field/FieldDeclare.oinca", Seq())
    assertResult(3)(res)
  }

  test("FieldInheritance") {
    val res = runProg("objectoriented/unittests/field/FieldInheritance.oinca", Seq())
    assertResult(10)(res)
  }

  test("ParamObject") {
    val res = runProg("objectoriented/unittests/ParamObject.oinca", Seq())
    assertResult(1)(res)
  }

  test("DynamicDispatch") {
    val res = runProg("objectoriented/unittests/DynamicDispatch.oinca", Seq())
    assertResult("BBC")(res)
  }

  test("MethodInheritance") {
    val res = runProg("objectoriented/unittests/MethodInheritance.oinca", Seq())
    assertResult(3)(res)
  }

  test("Mutability") {
    val res = runProg("objectoriented/unittests/Mutability.oinca", Seq())
    assertResult(true)(res)
  }

  test("Assignment") {
    val res = runProg("objectoriented/unittests/Assignment.oinca", Seq(3))
    assertResult(true)(res)
  }

  test("Constructor") {
    val res = runProg("objectoriented/unittests/Constructor.oinca", Seq(16, 8, 1))
    assertResult(3)(res)
  }

  test("Null") {
    val res = runProg("objectoriented/unittests/Null.oinca", Seq())
    assertResult(true)(res)
  }

  test("Plus") {
    val res = runProg("objectoriented/unittests/Plus.oinca", Seq())
    assertResult(5)(res)
  }

  test("Return") {
    val res1 = runProg("objectoriented/unittests/return/Return.oinca", Seq(true))
    assertResult(1)(res1)
    val res2 = runProg("objectoriented/unittests/return/Return.oinca", Seq(false))
    assertResult(2)(res2)
  }

  test("ReturnTwice") {
    val res1 = runProg("objectoriented/unittests/return/ReturnTwice.oinca", Seq())
    assertResult(1)(res1)
  }

  test("ReturnUnit") {
    val res1 = runProg("objectoriented/unittests/return/Unit.oinca", Seq())
    assertResult(())(res1)
  }

  test("ReturnImplicitUnit") {
    val res1 = runProg("objectoriented/unittests/return/ReturnImplicitUnit.oinca", Seq())
    assertResult(())(res1)
  }

  test("ReturnImplicitIf") {
    val res1 = runProg("objectoriented/unittests/return/ReturnImplicitIf.oinca", Seq(true))
    assertResult(1)(res1)
  }

  test("If") {
    val path = "objectoriented/unittests/if/If.oinca"
    assertResult(11)(runProg(path, Seq(true, true)))
    assertResult(7)(runProg(path, Seq(true, false)))
    assertResult(6)(runProg(path, Seq(false, false)))
  }

  test("IfDuplicate") {
    assertResult(10)(runProg("objectoriented/unittests/if/IfDuplicate.oinca", Seq(true, true)))
  }

  test("IfFalseTrue") {
    assertResult(true)(runProg("objectoriented/unittests/if/IfFalse.oinca", Seq()))
    assertResult(true)(runProg("objectoriented/unittests/if/IfTrue.oinca", Seq()))
  }

  test("Tuple") {
    assertResult(Seq(true, true, true))(runProg("objectoriented/unittests/Tuple.oinca", Seq()))
  }

  test("Super") {
    assertResult(10)(runProg("objectoriented/unittests/Super.oinca", Seq()))
  }
}
