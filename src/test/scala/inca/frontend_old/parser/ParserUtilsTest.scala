package inca.frontend_old.parser

import fastparse.Parsed._
import fastparse._
import org.scalatest.funsuite.AnyFunSuite

/**
  * Tests for the parser utilities
  *
  * @author Ronja Schnur (rschnur@students.uni-mainz.de)
  *         Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class ParserUtilsTest extends AnyFunSuite {

  test("test string empty") {
    parse("\"\"", ParserUtils.string(_)) match {
      case Success("", _) =>
      case _              => fail()
    }
  }

  test("test string containing single character ") {
    parse("\"a\"", ParserUtils.string(_)) match {
      case Success("a", _) =>
      case _               => fail()
    }
  }

  test("test string containing multiple characters") {
    parse("\"abc\"", ParserUtils.string(_)) match{
      case Success("abc", _) =>
      case _                 => fail()
    }
  }

  test("test string containing whitespace") {
    parse("\"  \"", ParserUtils.string(_)) match {
      case Success("  ", _) =>
      case _                => fail()
    }
  }
}
