package inca.frontend.objectoriented.parser

import inca.util.FileUtil
import inca.frontend.objectoriented.core.Module
import org.scalatest.funsuite.AnyFunSuite
import cats.parse.{Parser => P}
import org.scalatest.Assertion

import java.awt.Toolkit
import java.awt.datatransfer.{Clipboard, StringSelection}

class ParserTest extends AnyFunSuite {

  val parser: Parser = new Parser {}

  test("Simple syntax test") {
    val code = FileUtil.readFile("objectoriented/unittests/success/Playground.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("BinaryTree") {
    val code = FileUtil.readFile("objectoriented/unittests/success/BinaryTree.oinca")
    testSuccessAny(parser.module)(code)
  }

  private def testSuccessAny[T](parser: P[Module]): String => Assertion =
    (input: String) => {
       parser.parse(input) match {
         case Right((str, module)) =>
           println(module)

           // Copy the dot graph to the clipboard for debugging
           val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
           val selection = new StringSelection(module.dotString)
           clipboard.setContents(selection, selection)

           println()
           println("Remaining: ")
           println(str)
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