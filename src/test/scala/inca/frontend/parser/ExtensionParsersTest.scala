package inca.frontend.parser

import fastparse.Parsed.{Failure, Success}
import fastparse._
import inca.frontend.BaseFrontend
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.runtime.context.LanguageMetaInfo
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

class ExtensionParsersTest extends AnyFunSuite {

  test("test BoolOps") {
    val parser = new BaseFrontend(new LanguageMetaInfo()) with BoolOpsFrontend
    val testBoolOpsSuccess = testSuccess(parser.exp(_))
    val testBoolOpsFailure = testFailure(parser.exp(_))

    testBoolOpsSuccess("x && y", And(Var("x"), Var("y")))
    testBoolOpsSuccess("x && y && z", And(And(Var("x"), Var("y")), Var("z")))
    testBoolOpsSuccess("x &&y", And(Var("x"), Var("y")))
    testBoolOpsSuccess("x || y", Or(Var("x"), Var("y")))
    testBoolOpsSuccess("x || y || z", Or(Or(Var("x"), Var("y")), Var("z")))
    testBoolOpsSuccess("x|| y", Or(Var("x"), Var("y")))
    testBoolOpsSuccess("!x", Not(Var("x")))
    testBoolOpsSuccess("!(x && (y || z))", Not(And(Var("x"), Or(Var("y"), Var("z")))))
    testBoolOpsSuccess("!(x || (y && z))", Not(Or(Var("x"), And(Var("y"), Var("z")))))
    testBoolOpsSuccess("x || y && z", Or(Var("x"), And(Var("y"), Var("z"))))
    testBoolOpsSuccess("z && x || y", Or(And(Var("z"), Var("x")), Var("y")))
    testBoolOpsSuccess("z && x || y && z2", Or(And(Var("z"), Var("x")), And(Var("y"), Var("z2"))))
    testBoolOpsSuccess("!x && x", And(Not(Var("x")), Var("x")))

    testBoolOpsFailure("&& x")
    testBoolOpsFailure("|| y")
  }

  test("test Cast") {
    val parser = new BaseFrontend(new LanguageMetaInfo()) with CastFrontend
    val testCastSuccess = testSuccess(parser.exp(_))

    testCastSuccess("x:Int", Cast(Var("x"), TInt))
    testCastSuccess("x :Int", Cast(Var("x"), TInt))
    testCastSuccess("x : Int", Cast(Var("x"), TInt))
    testCastSuccess("x:Int:Int", Cast(Cast(Var("x"), TInt), TInt))
  }

  test("test Enum") {
    val parser = new BaseFrontend(new LanguageMetaInfo()) with EnumFrontend
    val testEnumSuccess = testSuccess(parser.exp(_))
    val testEnumFailure = testFailure(parser.exp(_))

    testEnumSuccess("enum(Int)", Enum(TInt))
    testEnumSuccess("enum(    Int)", Enum(TInt))
  }

  test("test ForallExists") {
    val parser = new BaseFrontend(new LanguageMetaInfo()) with ForallExistsFrontend
    val testForallExistsSuccess = testSuccess(parser.statement(_))
    val testForallExistsFailure = testFailure(parser.statement(_))

    testForallExistsSuccess(
      s"""forall v in (x, y) {
                |    assert x
                |}""".stripMargin,
      Forall(Name("v"), Tuple(Seq(Var("x"), Var("y"))), Body(Seq(Assert(Var("x")))))
    )

    testForallExistsSuccess(
      s"""exists v in list {
         |    assert v
         |}""".stripMargin,
      Exists(Name("v"), Var("list"), Body(Seq(Assert(Var("v")))))
    )

    testForallExistsFailure(
      s"""forall vin (x, y) {
         |    assert x
         |}""".stripMargin
    )

    testForallExistsFailure(
      s"""forall v in {
         |    assert x
         |}""".stripMargin
    )

    testForallExistsFailure(
      s"""forall v in(x, y) {
         |    assert x
         |}""".stripMargin
    )

    testForallExistsFailure(
      s"""exists v inlist {
         |    assert v
         |}""".stripMargin
    )

    testForallExistsFailure(
      s"""exists v in {
         |    assert v
         |}""".stripMargin
    )
  }

  test("test Foreach") {
    val parser = new BaseFrontend(new LanguageMetaInfo()) with ForeachFrontend
    val testForeachSuccess = testSuccess(parser.statement(_))
    val testForeachFailure = testFailure(parser.statement(_))

    testForeachSuccess(
      s"""foreach v in (x, y) {
                |    assert x
                |}""".stripMargin,
      Foreach(Name("v"), Tuple(Seq(Var("x"), Var("y"))), Body(Seq(Assert(Var("x")))))
    )

    testForeachFailure(
      s"""foreachv in (x, y) {
         |    assert x
         |}""".stripMargin
    )

    testForeachFailure(
      s"""foreach v in(x, y) {
         |    assert x
         |}""".stripMargin
    )

    testForeachFailure(
      s"""foreach v in {
         |    assert x
         |}""".stripMargin
    )
  }

  test("test IfThenElse") {
    val parser = new BaseFrontend(new LanguageMetaInfo()) with IfThenElseFrontend
    def testIfThenElse = testSuccess(parser.statement(_))

    testIfThenElse(
      s"""if (v) {
                |    assert x
                |} else if (q) {
                | val z = 7
                |} else {
                | assert y
                |}""".stripMargin,
      IfThenElse(
        Var("v"),
        Body(Seq(Assert(Var("x")))),
        Seq(ElseIf(Var("q"), Body(Seq(Assign(Seq(Name("z")), Constant(IntLiteral(7))))))),
        Some(Body(Seq(Assert(Var("y")))))
      )
    )
  }

  test("test Match") {
    val parser = new BaseFrontend(new LanguageMetaInfo()) with MatchFrontend
    def testMatch = testSuccess(parser.statement(_))

    testMatch(
      s"""|x match {
          |    case br0t(topping = cheese) => {}
          |    case x => { assert x == 5 }
          |    case (v, w) => {}
          |    case 5 => {}
          |    case x@y => {}
          |    case "Hello World" => {}
          |    case _ => {}
          |}""".stripMargin,
      Match(
        Var("x"),
        Seq(
          Case(
            NodePattern(
              TNode("br0t"),
              Seq(
                PatternBinding(Name("topping"), VarPattern(Name("cheese")))
              )
            ),
            Body(Seq.empty[Statement])
          ),
          Case(
            VarPattern(Name("x")),
            Body(Assert(Eq(Var("x"), Constant(IntLiteral(5)))))
          ),
          Case(
            TuplePattern(
              Seq(
                VarPattern(Name("v")),
                VarPattern(Name("w"))
              )
            ),
            Body(Seq.empty)
          ),
          Case(
            LiteralPattern(IntLiteral(5)),
            Body(Seq.empty)
          ),
          Case(
            NamedPattern(Name("x"), VarPattern(Name("y"))),
            Body(Seq.empty)
          ),
          Case(
            LiteralPattern(StringLiteral("Hello World")),
            Body(Seq.empty)
          ),
          Case(
            WildcardPattern,
            Body(Seq.empty)
          )
        )
      )
    )
  }

  test("test Switch") {
    val parser = new BaseFrontend(new LanguageMetaInfo()) with SwitchFrontend
    def testSwitch = testSuccess(parser.statement(_))

    testSwitch(s"""|switch {}""".stripMargin, Switch(Seq.empty))
    testSwitch(s"""switch {} union {}""", Switch(Seq(Body(Seq.empty), Body(Seq.empty))))
    testSwitch(
      s"""|switch{
          |    assert x
          |} union {}""".stripMargin,
      Switch(Seq(
        Body(Seq(Assert(Var("x")))),
        Body(Seq.empty))
      )
    )
    testSwitch(
      s"""|switch{
          |    val x = 5
          |} union {}""".stripMargin,
      Switch(Seq(
        Body(Seq(Assign(Seq(Name("x")), Constant(IntLiteral(5))))),
        Body(Seq.empty))
      )
    )
  }

  private def testSuccess[T](parser: P[_] => P[Any]): (String, T) => Assertion =
    (input: String, cmp: T) => {
      parse(input, parser) match {
        case Success(value, index)        =>
          assertResult(cmp)(value)
          assertResult(input.length)(index)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }

  private def testFailure[T](parser: P[_] => P[Any]): String => Unit =
    (input: String) => {
      parse(input, parser) match {
        case Success(value, index)        => fail(s"$value, $index")
        case Failure(label, index, extra) =>
      }
    }
}
