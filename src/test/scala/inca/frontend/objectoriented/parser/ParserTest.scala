package inca.frontend.objectoriented.parser

import inca.util.FileUtil
import inca.frontend.objectoriented.core.Module
import org.scalatest.funsuite.AnyFunSuite
import cats.parse.{Parser => P, Parser0 => P0}
import org.scalatest.Assertion

class ParserTest extends AnyFunSuite {

  val parser: Parser = new Parser {}

  test("Simple class") {
    val code = FileUtil.readFile("objectoriented/unittests/success/Plus.oinca")
    testSuccessAny(parser.module)(code)
  }

  private def testSuccessAny[T](parser: P[Module]): String => Assertion =
    (input: String) => {
       parser.parse(input) match {
         case Right((str, module)) =>
           println(module.dotString())
           println()
           println("Remaining: ")
           println(str)
           assertResult(0)(str.length)
         case Left(e) =>
           val offset = e.failedAtOffset
           println(s"Parsed to $offset: ")
           println(input.substring(offset))
           println(e.expected)//, e.offsets, e.failedAtOffset)
           fail(e.toString)
      }
    }
}