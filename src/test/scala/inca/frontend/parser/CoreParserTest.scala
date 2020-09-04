package inca.frontend.parser

import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import NoWhitespace._
import inca.frontend.core.Core._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import inca.frontend.core.Core

/**
  * Test class for the IncA core language parser @see CoreParser.
  *
  * @todo    unfinished
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class CoreParserTest extends AnyFunSuite {

  test("test TypeAnno") {
    def test_helper(t: TypeAnno) = {
      parse(t.prettyprint, CoreParser.typeanno(_)) match {
        case Success(value, index) => assert(value === t)
        case _: Failure            => fail()
      }
      parse(s" ${t.prettyprint}", CoreParser.typeanno(_)) match {
        case Success(value, index) => fail()
        case _: Failure            => {}
      }
    }

    Seq(
      TAny,
      TInt,
      TBool,
      TDouble,
      TLong,
      TString,
      TAnyLinked,
      TNode("t0mat3"),
      TNode("apf3l"),
      TNode("k1r5ch3")
    ).map(test_helper(_))
  }

  test("test Visibility") {
    def positive(v: Visibility)(t: String) =
      parse(t, CoreParser.visibility(_)) match {
        case _: Failure            => fail()
        case Success(value, index) => assert(value === v)
      }
    def negative(v: String) =
      parse(v, CoreParser.visibility(_)) match {
        case Failure(label, index, extra) => {}
        case Success(value, index)        => fail()
      }

    Seq("public", "   public", " public").map(positive(Public)(_))
    Seq("private", "   private", " private").map(positive(Private)(_))
    Seq("provate", "  prbplic", "   plsplapic", "bluplic").map(negative(_))
  }

  test("test TIterable") {
    parse("List[node]", CoreParser.titerable(_)) match {
      case Success(value, index) => {
        value match {
          case TEnumeration(contained) => fail()
          case TList(contained)        => assert(contained === TAnyLinked)
        }
      }
      case _: Failure => fail()
    }

    parse("List[apf3l]", CoreParser.titerable(_)) match {
      case Success(value, index) => {
        value match {
          case TEnumeration(contained) => fail()
          case TList(contained)        => assert(contained === TNode("apf3l"))
        }
      }
      case _: Failure => fail()
    }

    parse("Enum[node]", CoreParser.titerable(_)) match {
      case Success(value, index) => {
        value match {
          case TEnumeration(contained) => assert(contained === TAnyLinked)
          case TList(contained)        => fail()
        }
      }
      case _: Failure => fail()
    }

    parse("Enum[br0t]", CoreParser.titerable(_)) match {
      case Success(value, index) => {
        value match {
          case TEnumeration(contained) => assert(contained === TNode("br0t"))
          case TList(contained)        => fail()
        }
      }
      case _: Failure => fail()
    }

    parse("Enum[999]", CoreParser.titerable(_)) match {
      case Success(value, index) => fail()
      case _: Failure            => {}
    }

    parse("List[666]", CoreParser.titerable(_)) match {
      case Success(value, index) => fail()
      case _: Failure            => {}
    }
  }
}
