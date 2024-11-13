package inca.frontend.datalog.parser

import cats.parse.{Parser as P, Parser0 as P0}
import inca.frontend.datalog.syntax.*
import inca.ir.Name
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.ClassTag

class ParserTest extends AnyFunSuite {

  test("various") {
    testSuccessAny(Parser.literal)("1")
    testSuccessAny(Parser.literal)("12")
    testSuccessAny(Parser.literal)("123")
    testSuccessAny(Parser.literal)("123.456")
    testSuccessAny(Parser.param)("123.456")

    testSuccess(Parser.head)("Edge(1,2)",
      (Name("Edge"), List(Param.Constant(Literal.Int(1)), Param.Constant(Literal.Int(2))))
    )
    testSuccess(Parser.rule)("Edge(1,2).",
      (Name("Edge"), List(Rule(List(Param.Constant(Literal.Int(1)), Param.Constant(Literal.Int(2))), Seq())))
    )
  }

  test("Edge") {
    val m =
      s"""Edge(Int, Int).
         |Edge(1,2).
         |""".stripMargin
    testSuccessAny(Parser.relation)(m)

    val m2 =
      s"""Edge(Int, Int).
         |Edge(1,2).
         |Edge(2,3).
         |Edge(3,4).
         |Edge(4,5).
         |Edge(3,1).
         |""".stripMargin
    testSuccessAny(Parser.relation)(m2)

    val m3 =
      s"""Edge(Int, Int).
         |Edge(1,2).
         |Edge(1,2).
         |""".stripMargin
    testSuccessAny(Parser.relation)(m3)
  }

  test("Path") {
    val m =
      s"""Path(Int, Int).
         |Path(X,Y) :- Edge(X,Y)
         |          :- Edge(X,Z), Path(Z,Y).
         |""".stripMargin
    testSuccessAny(Parser.relation)(m)

    val m2 =
      s"""Edge(Int, Int).
         |Edge(1,2).
         |Edge(2,3).
         |Edge(3,4).
         |Edge(4,5).
         |Edge(3,1).
         |
         |Path(Int, Int).
         |Path(X,Y) :- Edge(X,Y)
         |          :- Edge(X,Z), Path(Z,Y).
         |""".stripMargin
    testSuccessAny(Parser.module)(m2)

    val m3 =
      s"""Edge(Int, Int).
         |Edge(1,2).
         |Edge(2,3).
         |Edge(3,4).
         |Edge(4,5).
         |Edge(3,1).
         |
         |Path(Int, Int).
         |Path(X,Y) :- Edge(X,Y).
         |Path(X,Z) :- Edge(X,Y), Path(Y,Z).
         |""".stripMargin
    testSuccessAny(Parser.module)(m3)
  }

  test("ShortestPath") {
    val m =
      s"""SPath(Int, Int, Int).
         |SPath(X,Y,min(n)) :- Edge(X,Y,n)
         |                  :- Edge(X,Z,n1), Path(Z,Y,n2), n == n1 + n2.
         |""".stripMargin
    testSuccessAny(Parser.relation)(m)
  }


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


  private def testSuccessAny[T](parser: P0[Any], silent: Boolean = false): String => Assertion =
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
}
