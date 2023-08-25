package inca.frontend.objectoriented.parser

import inca.util.FileUtil
import inca.frontend.objectoriented.core.Module
import org.scalatest.funsuite.AnyFunSuite
import cats.parse.{Parser => P}
import org.scalatest.Assertion

class ParserTest extends AnyFunSuite {

  val parser: Parser = new Parser {}

  test("Tuple") {
    val code = FileUtil.readFile("objectoriented/unittests/Tuple.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("Base 1") {
    val code = FileUtil.readFile("objectoriented/unittests/base/Base1.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("Base 2") {
    val code = FileUtil.readFile("objectoriented/unittests/base/Base2.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("Base 3") {
    val code = FileUtil.readFile("objectoriented/unittests/base/Base3.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("BinaryTree") {
    val code = FileUtil.readFile("objectoriented/graphs/BinaryTree.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("If") {
    val code = FileUtil.readFile("objectoriented/unittests/if/If.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("Set") {
    val code = FileUtil.readFile("objectoriented/unittests/set/Set.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("Plus") {
    val code = FileUtil.readFile("objectoriented/unittests/Plus.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("Generics 1") {
    val code = FileUtil.readFile("objectoriented/generics/GenericClassAndMethods.oinca")
    println("Code: ")
    println(code)
    println()

    println("Parsed: ")
    println(parser.module.parse(code))
    testSuccessAny(parser.module)(code)
  }

  private def testSuccessAny(parser: P[Module]): String => Assertion =
    (input: String) => {
       parser.parse(input) match {
         case Right((str, module)) =>
           assertResult(0)(str.length)
         case Left(e) =>
           val offset = e.failedAtOffset
           println(s"Parsed to $offset: ")
           println(input.substring(offset))
           println(e)//, e.offsets, e.failedAtOffset)
           fail(e.toString)
      }
    }
}