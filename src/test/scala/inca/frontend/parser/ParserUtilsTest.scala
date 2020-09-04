package inca.frontend.parser

import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import ParserUtils._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import inca.frontend.core.Core

/**
  * Tests for the parser utilities
  *
  * @todo implement identifier tests
  * @author Ronja Schnur (rschnur@students.uni-mainz.de)
  *         Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class ParserUtilsTest extends AnyFunSuite {

  test("test integer single digit") {
    parse("1", integer(_)) match {
      case Parsed.Success(1, _) => ()
      case _                    => fail()
    }
  }

  test("test integer zero") {
    parse("0", integer(_)) match {
      case Parsed.Success(0, _) => ()
      case _                    => fail()
    }
  }

  test("test integer multiple digits") {
    parse("123", integer(_)) match {
      case Parsed.Success(123, _) => ()
      case _                      => fail()
    }
  }

  test("test integer non digits input") {
    parse("f", integer(_)) match {
      case Parsed.Failure(_, 0, _) => ()
      case _                       => fail()
    }
  }

  test("test integer leading zeroes") {
    parse("01", integer(_)) match {
      case Parsed.Failure(_, _, _) => ()
      case Parsed.Success(v, _)    =>
        println(v)
        fail()
    }
  }

  test("test integer minus sign") {
    parse("-1", integer(_)) match {
      case Parsed.Success(-1, _) => ()
      case _                     => fail()
    }
  }

  test("test integer plus sign") {
    parse("+1", integer(_)) match {
      case Parsed.Success(1, _) => ()
      case _                    => fail()
    }
  }

  test("test identifier") {
    def positive(v: String) =
      parse(v, ParserUtils.identifier(_)) match {
        case Failure(label, index, extra) => fail()
        case Success(value, index)        => assert(value === v)
      }
    def negative(v: String) =
      parse(v, ParserUtils.identifier(_)) match {
        case Failure(label, index, extra) => {}
        case Success(value, index)        => fail()
      }

    Seq("kuch3n", "k3k53", "t33", "kl33", "br0t", "s0nn3nblum3").map(positive(_))
    Seq("3553n", "71nux", " ", "53h3n").map(negative(_))
  }

}
