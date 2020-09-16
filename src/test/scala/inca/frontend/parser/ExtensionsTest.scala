package inca.frontend.parser

import inca.frontend.core.Core
import inca.frontend.core.Core._
import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import inca.frontend.parser.extensions._
import inca.frontend.extensions._
import scala.annotation.switch
import scala.tools.nsc.interactive.Lexer.IntLit

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

    test_run("x:Int", Cast(Var("x"), TInt))
    test_run("x :Int", Cast(Var("x"), TInt))
    test_run("x : Int", Cast(Var("x"), TInt))
  }

  test("test Enum") {
    def test_run = test_helper(CoreParser(Seq(EnumParser)).exp(_))

    test_run("enum(Int)", Enum(TInt))
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

  test("test Match") {
    def test_run = test_helper(CoreParser(Seq(MatchParser)).statement(_))

    test_run(
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
                PatternBinding("topping", VarPattern("cheese"))
              )
            ),
            Body(Seq.empty[Statement])
          ),
          Case(
            VarPattern("x"),
            Body(Assert(Eq(Var("x"), Constant(IntLiteral(5)))))
          ),
          Case(
            TuplePattern(
              Seq(
                VarPattern("v"),
                VarPattern("w")
              )
            ),
            Body(Seq.empty)
          ),
          Case(
            LiteralPattern(IntLiteral(5)),
            Body(Seq.empty)
          ),
          Case(
            NamedPattern("x", VarPattern("y")),
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
    def test_run = test_helper(CoreParser(Seq(SwitchParser)).statement(_))

    test_run(s"""|switch {}""".stripMargin, Switch(Seq.empty))
    test_run(s"""switch {} union {}""", Switch(Seq(Body(Seq.empty), Body(Seq.empty))))
    test_run(s"""|switch{
                 |    assert x
                 |} union {}""".stripMargin, 
                 Switch(
                   Seq(
                     Body(Seq(Assert(Var("x")))),
                     Body(Seq.empty)
                   )
                 )
                 )
    test_run(s"""|switch{
                 |    val x = 5
                 |} union {}""".stripMargin, 
                 Switch(
                   Seq(
                     Body(Seq(Assign(Seq("x"), Constant(IntLiteral(5))))),
                     Body(Seq.empty)
                   )
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
