package inca.frontend.oodl.parser

import cats.parse.Parser as P
import inca.frontend.oodl.syntax.Parser
import inca.ir.Name
import inca.util.FileUtil
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.*
import scala.io.Source
import scala.jdk.StreamConverters.*
import scala.reflect.ClassTag

class ParserTest extends AnyFunSuite:

  val uri = classOf[ParserTest].getResource("/objectoriented").toURI;

  test("Parse all oodl IncA files") {
    Files.walkFileTree(Paths.get(uri), new FileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult =
        //println(s"Entering ${dir.getFileName}")
        FileVisitResult.CONTINUE
      override def visitFile(p: Path, attrs: BasicFileAttributes): FileVisitResult =
        if (p.toString.endsWith(".oodl")) {
          //println(s"Parsing $p")
          val file = Source.fromURI(p.toUri)
          val sourceCode = file.getLines().mkString("\n")
          file.close()
          testSuccessAny(Parser.module, false)(sourceCode)
        }
        FileVisitResult.CONTINUE
      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult =
        FileVisitResult.CONTINUE
      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult =
        //println(s"Leaving ${dir.getFileName}")
        FileVisitResult.CONTINUE
    })
  }

  /*test("numbers") {
    testSuccessAny(Parser.expression)("1")
    testSuccessAny(Parser.expression)("12")
    testSuccessAny(Parser.expression)("123")
    testSuccessAny(Parser.atomicExp)("123.456")

    testSuccessAny(Parser.additiveOperator)("+")
    testSuccessAny(Parser.multiplicativeOperator)("&")
    testSuccess(Parser.expression)("1 + 2", BinOp(IntLit(1), "+", IntLit(2)))
    testSuccess(Parser.expression)("100 + -233", BinOp(IntLit(100), "+", IntLit(-233)))

    testSuccess(Parser.expression)("1 + 2 + 3", BinOp(IntLit(1), "+", BinOp(IntLit(2), "+", IntLit(3))))
    testSuccess(Parser.expression)("1 * 2 * 3", BinOp(IntLit(1), "*", BinOp(IntLit(2), "*", IntLit(3))))

    testSuccess(Parser.expression)("(1 * 2) + 3", BinOp(BinOp(IntLit(1), "*", IntLit(2)), "+", IntLit(3)))
    testSuccess(Parser.expression)("1 + (2 * 3)", BinOp(IntLit(1), "+", BinOp(IntLit(2), "*", IntLit(3))))
    testSuccess(Parser.expression)("1 * 2 + 3", BinOp(BinOp(IntLit(1), "*", IntLit(2)), "+", IntLit(3)))
    testSuccess(Parser.expression)("1 + 2 * 3", BinOp(IntLit(1), "+", BinOp(IntLit(2), "*", IntLit(3))))

    testSuccess(Parser.expression)("x", Var(Name("x")))
    testSuccess(Parser.expression)("(x * 2) + 3", BinOp(BinOp(Var("x"), "*", IntLit(2)), "+", IntLit(3)))
    testSuccess(Parser.expression)("1 + (x * 3)", BinOp(IntLit(1), "+", BinOp(Var("x"), "*", IntLit(3))))
    testSuccess(Parser.expression)("1 * 2 + x", BinOp(BinOp(IntLit(1), "*", IntLit(2)), "+", Var("x")))
    testSuccess(Parser.expression)("x + 2 * 3", BinOp(Var("x"), "+", BinOp(IntLit(2), "*", IntLit(3))))
  }

  test("expressions") {
    testSuccessAny(Parser.expression)("let x = 1 + 2 in x")
    testSuccessAny(Parser.expression)("1 == 2")
    testSuccessAny(Parser.expression)("1 && 2")
    testSuccessAny(Parser.expression)("1 || 2")
    testSuccess(Parser.expression)("1 == 2 && 2 == 3", BinOp(BinOp(IntLit(1), "==", IntLit(2)), "&&", BinOp(IntLit(2), "==", IntLit(3))))
    testSuccess(Parser.expression)("1 == 2 || 2 == 3", BinOp(BinOp(IntLit(1), "==", IntLit(2)), "||", BinOp(IntLit(2), "==", IntLit(3))))

    testSuccessAny(Parser.unaryOperator)("-")
    testSuccessAny(Parser.unaryOperator ~ Parser.infixExp)("-x")
    testSuccessAny(Parser.expression)("-x")
    testSuccessAny(Parser.expression)("- x")

    testSuccessAny(Parser.expression)("foo(x)")
    testSuccessAny(Parser.expression)("(foo(x))(y)")
    testSuccessAny(Parser.expression)("foo(x)(y)")

    testSuccessAny(Parser.matchCase)("case Foo() => 1")
    testSuccessAny(Parser.expression)("m match { case Foo() => 1 }")
    testSuccessAny(Parser.expression)("plus(Succ(Succ(Succ(Zero()))), Succ(Succ(Zero())))")

    testSuccess(Parser.expression)("{}", SetExp(Seq()))
    testSuccess(Parser.expression)("{1}", SetExp(Seq(IntLit(1))))
    testSuccess(Parser.expression)("{1, 2, 3}", SetExp(Seq(IntLit(1), IntLit(2), IntLit(3))))

    testSuccess(Parser.expression)("x ++ y", BinOp(Var("x"), "++", Var("y")))
    testSuccessAny(Parser.expression)("flip()")
    testSuccessAny(Parser.expression)("flip() ++ flip()")

    testSuccessAny(Parser.lambdaVars)("(x: Int)")
    testSuccessAny(Parser.lambdaVars)("()")
    testSuccessAny(Parser.lambdaExp)("(x: Int) => x * 3")
    testSuccessAny(Parser.atomicExp)("(x: Int) => x * 3")
    testSuccessAny(Parser.atomicExp)("() => x * 3")

    testSuccessAny(Parser.expression)("{(1,2), (2,3), (3,1)}")
    testSuccessAny(Parser.expression)("transitive(() => {(1,2), (2,3), (3,1)})")

    testSuccessAny(Parser.expression)(
      """(exp match {
        |  case Var(s) => {}
        |  case Num(i) => {}
        |  case GreaterThan(e1, e2) => findExps(e1, f) ++ findExps(e2, f)
        |  case Mul(e1, e2) => findExps(e1, f) ++ findExps(e2, f)
        |  case Add(e1, e2) => findExps(e1, f) ++ findExps(e2, f)
        |  case Sub(e1, e2) => findExps(e1, f) ++ findExps(e2, f)
        |}) ++ (if (f(exp)) {exp} else {})
        |""".stripMargin
    )
    testSuccessAny(Parser.expression)("let cfg = () => flow(prog) in transitive(cfg)")
    testSuccessAny(Parser.compareOperator)("<")
    testSuccessAny(Parser.compareOperator)("<=")
    testSuccessAny(Parser.expression)("5 <= 10")
  }

  test("Fold test") {
    testSuccessAny(Parser.expression)("0")
    testSuccessAny(Parser.expression)("add")
    testSuccessAny(Parser.expression)("fromTo(start, end)")
    testSuccessAny(Parser.foldExp)("fold(0, add, fromTo(start, end))")
  }


  test("Module test") {
    val boolDef = DataDef(Seq(), None, Name("Bool"), Seq(), Seq(DataConstructor(Name("True"), Seq()), DataConstructor(Name("False"), Seq())))
    val funDef = FunctionDef(Seq(), None, Name("neg"), Seq(), Seq(Param(Name("b"), TName(Name("Bool")))), TName(Name("Bool")),
      Match(Var("b"), Seq(
        (ConstructorPattern(Name("True"), Seq()), Call(Var(Name("False")), Seq(), Seq())),
        (ConstructorPattern(Name("False"), Seq()), Call(Var(Name("True")), Seq(), Seq())))))
    val moduleDef = Module(Name("Main"), Seq(), Seq(boolDef, funDef))
    val moduleString =
      """module Main
        |data Bool = True() | False()
        |def neg(b: Bool): Bool = b match {
        |  case True() => False()
        |  case False() => True()
        |}
        |""".stripMargin

    testSuccess(Parser.module)(moduleString, moduleDef)
  }

  test("FunctionDef test") {
    val funDef = FunctionDef(Seq(), None, Name("foo"), Seq(), Seq(Param(Name("x"), TInt)), TInt, If(Var("x"), IntLit(1), IntLit(2)))
    testSuccess(Parser.functionDef)("def foo(x: Int): Int = if (x) 1 else 2", funDef)

    val annoFunDef = FunctionDef(Seq(MainFunctionAnno()), None, Name("foo"), Seq(), Seq(Param(Name("x"), TInt)), TInt, If(Var("x"), IntLit(1), IntLit(2)))
    val annoFunString = "@main def foo(x: Int): Int = if (x) 1 else 2"
    Parser.functionDef.parse(annoFunString) match {
      case Right((_, value)) =>
        assert(value == annoFunDef)
        assert(value.hasAnnotation(MainFunctionAnno.KEY))
      case _ => assert(false)
    }
  }

  test("DataDef test") {
    val peanoDef = DataDef(Seq(), None, Name("Nat"), Seq(),
      Seq(
        DataConstructor(Name("Zero"), Seq()),
        DataConstructor(Name("Succ"), Seq(TName(Name("Nat"))))))
    testSuccess(Parser.dataDef)("data Nat = Zero() | Succ(Nat)", peanoDef)

    val expDef = DataDef(Seq(), None, Name("Exp"), Seq(),
      Seq(
        DataConstructor(Name("Num"), Seq(TName(Name("Int")))),
        DataConstructor(Name("Add"), Seq(TName(Name("Exp")), TName(Name("Exp"))))))
    testSuccess(Parser.dataDef)("data Exp = Num(Int) | Add(Exp, Exp)", expDef)
  }

  test("Expression test") {
    testSuccess(Parser.expression)("test", Var("test"))
    testSuccess(Parser.expression)("if (test) x else y", If(Var("test"), Var("x"), Var("y")))

    testSuccess(Parser.expression)("(x, z)", Tuple(Seq(Var("x"), Var("z"))))

    testSuccess(Parser.expression)("let x = y in x", Let(Seq(Name("x")), None, Var("y"), Var("x")))
    testSuccess(Parser.expression)("let x: Any = y in x", Let(Seq(Name("x")), Some(TAny), Var("y"), Var("x")))
    testSuccess(Parser.expression)("let (x, y) = tuple in (y, x)", Let(Seq(Name("x"), Name("y")), None, Var("tuple"), Tuple(Seq(Var("y"), Var("x")))))

    val letExp = Let(Seq(Name("x"), Name("y")), Some(TTuple(Seq(TAny, TNothing))), Var("tuple"), Tuple(Seq(Var("y"), Var("x"))))
    testSuccess(Parser.expression)("let (x, y): (Any, Nothing) = tuple in (y, x)", letExp)

    testSuccess(Parser.expression)("foo(x)", Call(Var(Name("foo")), Seq(), Seq(Var("x"))))
    testSuccess(Parser.expression)("foo(x, (y, z))", Call(Var(Name("foo")), Seq(), Seq(Var("x"), Tuple(Seq(Var("y"), Var("z"))))))

    testSuccess(Parser.expression)("((x, y))", Tuple(Seq(Var("x"), Var("y"))))

    testSuccess(Parser.expression)("1", IntLit(1))
    testSuccess(Parser.expression)(""""ABC"""", StringLit("ABC"))

    testSuccess(Parser.infixExp)("x + y", BinOp(Var("x"), "+", Var("y")))


    val matchString =
      """b match {
        |  case True() => False()
        |  case False() => True()
        |}
        |""".stripMargin
    val matchExp = Match(Var("b"), Seq(
      (ConstructorPattern(Name("True"), Seq()), Call(Var(Name("False")), Seq(), Seq())),
      (ConstructorPattern(Name("False"), Seq()), Call(Var(Name("True")), Seq(), Seq()))))
    testSuccess(Parser.expression)(matchString, matchExp)
  }*/

  private def testSuccess[T](parser: P[T]): (String, T) => Assertion =
    (input: String, cmp: T) => {
      parser.parseAll(input) match
        case Left(value) => assert(false, explainParseError(value))
        case Right(value) => assertResult(cmp)(value)
    }

  def explainParseError(e: P.Error): String =
    val input = e.input.getOrElse("")
    val startLineIndex = input.lastIndexOf('\n', e.failedAtOffset - 1).max(0)
    val endLineIndex = input.indexOf('\n', e.failedAtOffset + 1)
    val relevant = input.substring(startLineIndex, if (endLineIndex == -1) input.length else endLineIndex)
    s"""Parse error, expected ${e.expected.toList} in
       |$relevant
       |""".stripMargin


  private def testSuccessAny[T](parser: P[Any], silent: Boolean = false): String => Assertion =
    (input: String) => {
      parser.parseAll(input) match
        case Left(value) => assert(false, explainParseError(value))
        case Right(value) =>
          if (!silent) println(value)
          assert(true)
    }

  private def testFailure[T <: AnyRef](parser: P[Any])(using ClassTag[T]): String => Unit =
    (input: String) => {
      assertThrows[T](parser.parseAll(input))
    }
