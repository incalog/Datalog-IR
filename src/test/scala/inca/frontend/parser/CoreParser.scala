package inca.frontend.parser

import org.scalatest.funsuite.AnyFunSuite
import fastparse._ 
import NoWhitespace._
import inca.frontend.core.Core._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

/**
  * Test class for the IncA core language parser @see CoreParser.
  * 
  * @todo    unfinished
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class CoreParserTest extends AnyFunSuite
{
    test("Test TypeAnno"){
        def test_helper(t : TypeAnno) = {
            parse(t.prettyprint, CoreParser.typeanno(_)) match  {
                case Success(value, index) => assert(value === t)
                case _: Failure => fail()
            }
            parse(s" ${t.prettyprint}", CoreParser.typeanno(_)) match  {
                case Success(value, index) => fail()
                case _: Failure => {}
            }
        }

        test_helper(TAny)
        test_helper(TInt)
        test_helper(TBool)
        test_helper(TDouble)
        test_helper(TLong)
        test_helper(TString)
    }

    test("Test Visibility") {
        parse("public", CoreParser.visibility(_)) match {
            case _: Failure => fail()
            case Success(value, index) => assert(value === Public)
        }
        parse("    public", CoreParser.visibility(_)) match {
            case _: Failure => fail()
            case Success(value, index) => assert(value === Public)
        }
        parse("private", CoreParser.visibility(_)) match {
            case _: Failure => fail()
            case Success(value, index) => assert(value === Private)
        }
        parse("   private", CoreParser.visibility(_)) match {
            case _: Failure => fail()
            case Success(value, index) => assert(value === Private)
        }
        parse("provate", CoreParser.visibility(_)) match {
            case _: Failure => {}
            case Success(value, index) => fail()
        }
        parse("prpublic", CoreParser.visibility(_)) match {
            case _: Failure => {}
            case Success(value, index) => fail()
        }
    }
}