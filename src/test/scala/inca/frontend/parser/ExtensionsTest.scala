package inca.frontend.parser

import inca.frontend.core.Core
import inca.frontend.core.Core._
import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import inca.frontend.parser.extensions._
import inca.frontend.extensions._

class ExtensionsTest extends AnyFunSuite {

  test("test BoolOps") {
    def test_run = test_helper(CoreParser(Seq(BoolOpsParser)).exp(_))

    test_run("x && y", And(Var("x"), Var("y")))
    test_run("x || y", Or(Var("x"), Var("y")))
    test_run("!x", Not(Var("x")))
    test_run("!(x && (y || z))", Not(And(Var("x"), Or(Var("y"), Var("z")))))
  }

  test("test Cast") {
    def test_run = test_helper(CoreParser(Seq(CastParser)).exp(_))

    test_run("x:int", Cast(Var("x"), TInt))
    test_run("x :int", Cast(Var("x"), TInt))
    test_run("x : int", Cast(Var("x"), TInt))
  }

  test("test Enum") {
    def test_run = test_helper(CoreParser(Seq(EnumParser)).exp(_))

    test_run("enum(int)", Enum(TInt))
  }

  test("test ForallExists") {
    def test_run = test_helper(CoreParser(Seq(ForallExistsParser)).statement(_))

    test_run(
      s"""forall v in (x, y) {
                |    assert x
                |}""".stripMargin,
      Forall("v", Tuple(Seq(Var("x"), Var("y"))), Body(Seq(Assert(Var("x")))))
    )
  }

  test("test Foreach") {
    def test_run = test_helper(CoreParser(Seq(ForeachParser)).statement(_))

    test_run(
      s"""foreach v in (x, y) {
                |    assert x
                |}""".stripMargin,
      Foreach("v", Tuple(Seq(Var("x"), Var("y"))), Body(Seq(Assert(Var("x")))))
    )
  }

  test("test IfThenElse") {
    def test_run = test_helper(CoreParser(Seq(IfThenElseParser)).statement(_))

    test_run(
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
        Seq(ElseIf(Var("q"), Body(Seq(Assign(Seq("z"), Constant(IntLiteral(7))))))),
        Some(Body(Seq(Assert(Var("y")))))
      )
    )

  }

  private def test_helper[T](parser: P[_] => P[Any]) =
    (input: String, cmp: T) => {
      parse(input, parser) match {
        case Success(value, index)        => assert(cmp === value)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }

  private[parser] def test_helper_negative[T](parser: P[_] => P[Any]) =
    (input: String) => {
      parse(input, parser) match {
        case Success(value, index)        => fail(s"$value, $index")
        case Failure(label, index, extra) =>
      }
    }
}
