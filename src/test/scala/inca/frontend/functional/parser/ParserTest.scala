package inca.frontend.functional.parser

import fastparse.Parsed.{Failure, Success}
import fastparse.{P, parse}
import inca.examples.functional.{AST, Code, ControlDataFlow, HigherOrder}
import inca.frontend.functional.core._
import inca.util.Scala
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.quasiquotes._

class ParserTest extends AnyFunSuite {

  val parser: Parser = new Parser {}

  test("base example") {
    testSuccess(parser.module(_))(Code.baseExample, AST.baseExample)
  }

  test("base example 2a") {
    testSuccess(parser.module(_))(Code.baseExample2a, AST.baseExample2)
  }

  test("base example 2b") {
    testSuccess(parser.module(_))(Code.baseExample2b, AST.baseExample2)
  }

  test("var example") {
    testSuccess(parser.module(_))(Code.varExample, AST.varExample)
  }

  test("if example") {
    testSuccess(parser.module(_))(Code.ifExample, AST.ifExample)
  }

  test("if example 2") {
    testSuccess(parser.module(_))(Code.ifExample2, AST.ifExample2)
  }

  test("inc example") {
    testSuccess(parser.module(_))(Code.incModule, AST.incModule)
  }

  test("fact example") {
    testSuccess(parser.module(_))(Code.factModule, AST.factModule)
  }

  test("plus example") {
    testSuccess(parser.module(_))(Code.plusModule, AST.plusModule)
  }

  test("plus real example") {
    testSuccess(parser.module(_))(Code.plusRealModule, AST.plusRealModule)
  }

  test("Module test") {
    val boolDef = DataDef(Seq(), None, Name("Bool"), Seq(DataConstructor(Name("True"), Seq()), DataConstructor(Name("False"), Seq())))
    val funDef = FunctionDef(Seq(), None, Name("neg"), Seq(Param(Name("b"), TData(Name("Bool")))), TData(Name("Bool")),
      Match(Var("b"), Seq(
        (ConstructorPattern(Name("True"), Seq()), Call(Var(Name("False")), Seq())),
        (ConstructorPattern(Name("False"), Seq()), Call(Var(Name("True")), Seq())))))
    val moduleDef = Module(Name("Main"), Seq(), Seq(boolDef, funDef))
    val moduleString =
      """module Main
        |data Bool = True() | False()
        |def neg(b: Bool): Bool = b match {
        |  case True() => False()
        |  case False() => True()
        |}
        |""".stripMargin

    testSuccess(parser.module(_))(moduleString, moduleDef)

  }

  test("FunctionDef test") {
    val funDef = FunctionDef(Seq(), None, Name("foo"), Seq(Param(Name("x"), TScala("Int"))), TScala("Int"), If(Var("x"), BaseLit(Scala(q"1")), BaseLit(Scala(q"2"))))
    testSuccess(parser.functionDef(_))("def foo(x: Int): Int = if (x) `1` else `2`", funDef)

    val annoFunDef = FunctionDef(Seq(MainFunctionAnno), None, Name("foo"), Seq(Param(Name("x"), TScala("Int"))), TScala("Int"), If(Var("x"), BaseLit(Scala(q"1")), BaseLit(Scala(q"2"))))
    val annoFunString = "@main def foo(x: Int): Int = if (x) `1` else `2`"
    parse(annoFunString, parser.functionDef(_)) match {
      case Success(value, _) =>
        assert(value == annoFunDef)
        assert(value.hasAnnotation(MainFunctionAnno.key))
      case Failure(_, _, _) => assert(false)
    }
  }

  test("DataDef test") {
    val peanoDef = DataDef(Seq(), None, Name("Nat"),
      Seq(
        DataConstructor(Name("Zero"), Seq()),
        DataConstructor(Name("Succ"), Seq(TData(Name("Nat"))))))
    testSuccess(parser.dataDef(_))("data Nat = Zero() | Succ(Nat)", peanoDef)

    val expDef = DataDef(Seq(), None, Name("Exp"),
      Seq(
        DataConstructor(Name("Num"), Seq(TScala("Int"))),
        DataConstructor(Name("Add"), Seq(TData(Name("Exp")), TData(Name("Exp"))))))
    testSuccess(parser.dataDef(_))("data Exp = Num(Int) | Add(Exp, Exp)", expDef)
  }

  test("Expression test") {
    testSuccess(parser.exp(_))("test", Var("test"))
    testSuccess(parser.exp(_))("if (test) x else y", If(Var("test"), Var("x"), Var("y")))

    testSuccess(parser.exp(_))("(x, z)", Tuple(Seq(Var("x"), Var("z"))))

    testSuccess(parser.exp(_))("let x = y in x", Let(Seq(Name("x")), None, Var("y"), Var("x")))
    testSuccess(parser.exp(_))("let x: Any = y in x", Let(Seq(Name("x")), Some(TAny), Var("y"), Var("x")))
    testSuccess(parser.exp(_))("let (x, y) = tuple in (y, x)", Let(Seq(Name("x"), Name("y")), None, Var("tuple"), Tuple(Seq(Var("y"), Var("x")))))

    val letExp = Let(Seq(Name("x"), Name("y")), Some(TTuple(Seq(TAny, TNothing))), Var("tuple"), Tuple(Seq(Var("y"), Var("x"))))
    testSuccess(parser.exp(_))("let (x, y): (Any, Nothing) = tuple in (y, x)", letExp)

    testSuccess(parser.exp(_))("foo(x)", Call(Var(Name("foo")), Seq(Var("x"))))
    testSuccess(parser.exp(_))("foo(x, (y, z))", Call(Var(Name("foo")), Seq(Var("x"), Tuple(Seq(Var("y"), Var("z"))))))

    testSuccess(parser.exp(_))("((x, y))", Tuple(Seq(Var("x"), Var("y"))))

    testSuccess(parser.exp(_))("`1`", BaseLit(Scala(q"1")))
    testSuccess(parser.exp(_))("""`"ABC"`""", BaseLit(Scala(q""""ABC"""")))
    println(BaseApplyInfix(Var("x"), Scala(meta.Term.Name("+")), Var("y")))

    testSuccess(parser.baseApplyInfixExp(_))("x + y", BaseApplyInfix(Var("x"), Scala(meta.Term.Name("+")), Var("y")))


    val matchString =
      """b match {
        |  case True() => False()
        |  case False() => True()
        |}
        |""".stripMargin
    val matchExp = Match(Var("b"), Seq(
      (ConstructorPattern(Name("True"), Seq()), Call(Var(Name("False")), Seq())),
      (ConstructorPattern(Name("False"), Seq()), Call(Var(Name("True")), Seq()))))
    testSuccess(parser.exp(_))(matchString, matchExp)
  }

  test("set constants") {
    testSuccessAny(parser.module(_))(Code.setConstModule)
  }

  test("set operations") {
    testSuccessAny(parser.module(_))(Code.setOperationsModule)
  }

  test("cflow") {
    testSuccessAny(parser.module(_))(ControlDataFlow.cflowModule)
  }

  test("available expressions") {
    testSuccessAny(parser.module(_))(ControlDataFlow.AEModule)
  }

  test("reaching definitions") {
    testSuccessAny(parser.module(_))(ControlDataFlow.RDmodule)
  }

  test("applyFun") {
    testSuccessAny(parser.module(_))(HigherOrder.applyFun)
  }

  test("lambda") {
    testSuccessAny(parser.module(_))(HigherOrder.lambda)
  }

  test("lambdaHigherOrder") {
    testSuccessAny(parser.module(_))(HigherOrder.lambdaHigherOrder)
  }

  test("composeFun") {
    testSuccessAny(parser.module(_))(HigherOrder.composeFun)
  }

  test("composeLambdas") {
    testSuccessAny(parser.module(_))(HigherOrder.composeLambdas)
  }

  test("transitiveWrong") {
    testSuccessAny(parser.module(_))(HigherOrder.transitiveWrong)
  }

  test("transitive") {
    testSuccessAny(parser.module(_))(HigherOrder.transitive)
  }

  test("data types") {
    testSuccessAny(parser.module(_))(
      s"""module Main
         |data Interval = IV(Int, Int) | TopInterval()
         |data Bool = True() | False() | TopBool()
         |data Val = BotVal() | IntervalVal(Interval) | BoolVal(Bool) | TopVal()
         |""".stripMargin
    )
  }

  test("set union") {
    testSuccessAny(parser.module(_))(
      s"""module Main
         |
         |def test(): Set[Int] = {1, 2, 3} & {1, 3}
         |""".stripMargin
    )
  }

  private def testSuccess[T](parser: P[_] => P[Any]): (String, T) => Assertion =
    (input: String, cmp: T) => {
      parse(input, parser) match {
        case Success(value, index)        =>
          println(value)
          assert(value === cmp)
          assertResult(input.length)(index)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }

  private def testSuccessAny[T](parser: P[_] => P[Any]): String => Assertion =
    (input: String) => {
      parse(input, parser) match {
        case Success(value, index)        =>
          println(value)
          assertResult(input.length)(index)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }

  private def testFailure[T](parser: P[_] => P[Any]): String => Unit =
    (input: String) => {
      parse(input, parser) match {
        case Success(value, index) if input.length == index => fail(s"Expected failed parsing, but got $value")
        case Success(value, index) if input.length != index =>
        case Failure(label, index, extra) =>
      }
    }
}
