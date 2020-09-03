package inca.frontend.parser

import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import ParserUtils._

/**
 * Tests for the parser utilities
 *
 * @todo implement identifier tests
 * @author Ronja Schnur (rschnur@students.uni-mainz.de)
 *         Julian Cichorius (jcichori@students.uni-mainz.de)
 */
class ParserUtilsTest extends AnyFunSuite{

  test("test integer single digit") {
    parse("1", integer(_)) match {
      case Parsed.Success(1, _) => ()
      case _ => fail()
    }
  }

  test("test integer zero") {
    parse("0", integer(_)) match {
      case Parsed.Success(0, _) => ()
      case _ => fail()
    }
  }

  test("test integer multiple digits") {
    parse("123", integer(_)) match {
      case Parsed.Success(123, _) => ()
      case _ => fail()
    }
  }

  test("test integer non digits input") {
    parse("f", integer(_)) match {
      case Parsed.Failure(_, 0, _) => ()
      case _ => fail()
    }
  }

  test("test integer leading zeroes") {
    parse("01", integer(_)) match {
      case Parsed.Failure(_, _, _) => ()
      case Parsed.Success(v, _) =>
        println(v)
        fail()
    }
  }

}
