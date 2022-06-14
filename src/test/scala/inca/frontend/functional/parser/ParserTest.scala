package inca.frontend.functional.parser

import fastparse.Parsed.{Failure, Success}
import fastparse.{P, parse}
import inca.frontend.functional.core._
import inca.util.{FileUtil, Scala}
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.quasiquotes._

class ParserTest extends AnyFunSuite {

  val parser: Parser = new Parser {}

  test("base example 1") {
    val code = FileUtil.readFile("functional/unittests/Base1.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("base example 2") {
    val code = FileUtil.readFile("functional/unittests/Base2.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("base example 3") {
    val code = FileUtil.readFile("functional/unittests/Base3.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("var example") {
    val code = FileUtil.readFile("functional/unittests/Var.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("if example") {
    val code = FileUtil.readFile("functional/unittests/If.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("if example 2") {
    val code = FileUtil.readFile("functional/unittests/If2.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("inc example") {
    val code = FileUtil.readFile("functional/unittests/Inc.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("fact example") {
    val code = FileUtil.readFile("functional/unittests/Fact.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("plus example") {
    val code = FileUtil.readFile("functional/unittests/Plus.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("plus real example") {
    val code = FileUtil.readFile("functional/unittests/PlusReal.finca")
    testSuccessAny(parser.module(_))(code)
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
    val code = FileUtil.readFile("functional/unittests/SetConst.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("set operations") {
    val code = FileUtil.readFile("functional/unittests/SetOps.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("available expressions") {
    val code = FileUtil.readFile("functional/controlflow/AvailableExpressions.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("reaching definitions") {
    val code = FileUtil.readFile("functional/controlflow/ReachingDefinition.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("applyFun") {
    val code = FileUtil.readFile("functional/higherorder/Apply.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("lambda") {
    val code = FileUtil.readFile("functional/higherorder/Lambda.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("lambdaHigherOrder") {
    val code = FileUtil.readFile("functional/higherorder/LambdaHigherOrder.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("composeFun") {
    val code = FileUtil.readFile("functional/higherorder/ComposeFun.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("composeLambdas") {
    val code = FileUtil.readFile("functional/higherorder/ComposeLambda.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("transitiveWrong") {
    val code = FileUtil.readFile("functional/higherorder/TransitiveWrong.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("transitive") {
    val code = FileUtil.readFile("functional/higherorder/Transitive.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("data types") {
    val code = FileUtil.readFile("functional/unittests/Datatypes.finca")
    testSuccessAny(parser.module(_))(code)
  }

  test("set intersection") {
    val code = FileUtil.readFile("functional/unittests/SetIntersection.finca")
    testSuccessAny(parser.module(_))(code)
  }

  private def testSuccess[T](parser: P[_] => P[Any]): (String, T) => Assertion =
    (input: String, cmp: T) => {
      parse(input, parser) match {
        case Success(value, index)        =>
          assert(value === cmp)
          assertResult(input.length)(index)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }

  private def testSuccessAny[T](parser: P[_] => P[Any]): String => Assertion =
    (input: String) => {
      parse(input, parser) match {
        case Success(value, index)        =>
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
