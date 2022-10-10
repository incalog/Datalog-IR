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

  test("Tuple") {
    val code = FileUtil.readFile("objectoriented/unittests/Tuple.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("Simple syntax test") {
    val code = FileUtil.readFile("objectoriented/parser/Playground.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("Base 1") {
    val code = FileUtil.readFile("objectoriented/unittests/Base1.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("Base 2") {
    val code = FileUtil.readFile("objectoriented/unittests/Base2.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("Base 3") {
    val code = FileUtil.readFile("objectoriented/unittests/Base3.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("BinaryTree") {
    val code = FileUtil.readFile("objectoriented/unittests/BinaryTree2.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("If") {
    val code = FileUtil.readFile("objectoriented/unittests/If.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("Set") {
    val code = FileUtil.readFile("objectoriented/unittests/Set.oinca")
    testSuccessAny(parser.module)(code)
  }

  test("Plus") {
    val code = FileUtil.readFile("objectoriented/unittests/Plus.oinca")
    testSuccessAny(parser.module)(code)
  }

  private def testSuccessAny[T](parser: P[Module]): String => Assertion =
    (input: String) => {
       parser.parse(input) match {
         case Right((str, module)) =>
           println(module)

           import inca.frontend.objectoriented.analyze.AbstractSyntaxTree
           val ast = new AbstractSyntaxTree(module)

           import java.awt.Toolkit
           import java.awt.datatransfer.{Clipboard, StringSelection}
           val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
           val selection = new StringSelection(ast.toGraphViz)
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