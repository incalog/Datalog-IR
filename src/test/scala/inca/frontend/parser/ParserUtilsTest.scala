package inca.frontend.parser

import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import ParserUtils._
import fastparse.Parsed._

/**
  * Tests for the parser utilities
  *
  * @author Ronja Schnur (rschnur@students.uni-mainz.de)
  *         Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class ParserUtilsTest extends AnyFunSuite {

  test("test integer single digit") {
    parse("1", integer(_)) match {
      case Success(1, _) => ()
      case _                    => fail()
    }
  }

  test("test integer zero") {
    parse("0", integer(_)) match {
      case Success(0, _) => ()
      case _                    => fail()
    }
  }

  test("test integer multiple digits") {
    parse("123", integer(_)) match {
      case Success(123, _) => ()
      case _                      => fail()
    }
  }

  test("test integer non digits input") {
    parse("f", integer(_)) match {
      case Failure(_, 0, _) => ()
      case _                       => fail()
    }
  }

  test("test integer leading zeroes") {
    parse("01", integer(_)) match {
      case Failure(_, _, _) => ()
      case Success(v, _)    =>
        fail(s"$v")
    }
  }

  test("test integer minus sign") {
    parse("-1", integer(_)) match {
      case Success(-1, _) => ()
      case _                     => fail()
    }
  }

  test("test integer plus sign") {
    parse("+1", integer(_)) match {
      case Success(1, _) => ()
      case _                    => fail()
    }
  }

  test("test double simple") {
    parse("1.0", double(_)) match {
      case Success(1.0, _) =>
      case _                      => fail()
    }
  }

  test("test double start zero") {
    parse("0.2", double(_)) match {
      case Success(0.2, _) =>
      case _                      => fail()
    }
  }

  test("test double integer with d") {
    parse("2d", double(_)) match {
      case Success(2d, _) =>
      case _                     => fail()
    }
  }

  test("test double multiple post-dot digits") {
    parse("0.128", double(_)) match {
      case Success(0.128, _) =>
      case _                        => fail()
    }
  }

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
