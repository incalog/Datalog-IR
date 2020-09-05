package inca.frontend.parser

import org.scalatest.funsuite.AnyFunSuite
import fastparse._
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
class CoreParserTest extends AnyFunSuite {

  test("test TypeAnno") {
    def test_helper(t: TypeAnno) = {
      parse(t.prettyprint, CoreParser.typeAnno(_)) match {
        case Success(value, index) => assert(value === t)
        case _: Failure            => fail()
      }
      parse(s" ${t.prettyprint}", CoreParser.typeAnno(_)) match {
        case Success(value, index) => fail()
        case _: Failure            =>
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
        case Failure(label, index, extra) =>
        case Success(value, index)        => fail()
      }

    Seq("public", "   public", " public").map(positive(Public)(_))
    Seq("private", "   private", " private").map(positive(Private)(_))
    Seq("provate", "  prbplic", "   plsplapic", "bluplic").map(negative)
  }

  test("test TIterable") {
    parse("List[node]", CoreParser.tIterable(_)) match {
      case Success(value, index) =>
        value match {
          case TEnumeration(contained) => fail()
          case TList(contained)        => assert(contained === TAnyLinked)
        }
      case _: Failure => fail()
    }

    parse("List[apf3l]", CoreParser.tIterable(_)) match {
      case Success(value, index) =>
        value match {
          case TEnumeration(contained) => fail()
          case TList(contained)        => assert(contained === TNode("apf3l"))
        }
      case _: Failure => fail()
    }

    parse("Enum[node]", CoreParser.tIterable(_)) match {
      case Success(value, index) =>
        value match {
          case TEnumeration(contained) => assert(contained === TAnyLinked)
          case TList(contained)        => fail()
        }
      case _: Failure => fail()
    }

    parse("Enum[br0t]", CoreParser.tIterable(_)) match {
      case Success(value, index) =>
        value match {
          case TEnumeration(contained) => assert(contained === TNode("br0t"))
          case TList(contained)        => fail()
        }
      case _: Failure => fail()
    }

    parse("Enum[999]", CoreParser.tIterable(_)) match {
      case Success(value, index) => fail()
      case _: Failure            =>
    }

    parse("List[666]", CoreParser.tIterable(_)) match {
      case Success(value, index) => fail()
      case _: Failure            =>
    }
  }

  test("test IntLiteral") {
    parse("1", CoreParser.intLiteral(_)) match {
      case Success(IntLiteral(1), _) =>
      case _                         => fail()
    }
  }

  test("test BooleanLiteral") {
    def check(b: Boolean): Unit = {
      parse(b.toString, CoreParser.booleanLiteral(_)) match {
        case Success(BooleanLiteral(bool), _) if bool == b =>
        case _                                             => fail()
      }
    }
    check(true)
    check(false)
  }

  test("test LongLiteral") {
    parse("1L", CoreParser.longLiteral(_)) match {
      case Success(LongLiteral(1), _) =>
      case _                          => fail()
    }
  }

  test("test DoubleLiteral") {
    parse("1.0", CoreParser.doubleLiteral(_)) match {
      case Success(DoubleLiteral(1d), _) =>
      case _                             => fail()
    }
  }

  test("test UnitLiteral") {
    parse("unit", CoreParser.unitLiteral(_)) match {
      case Success(UnitLiteral, _) =>
      case _                       => fail()
    }
  }

  test("test Param") {
    parse(s"param:${TBool.prettyprint}", CoreParser.param(_)) match {
      case Success(Param("param", TBool), _) =>
      case _                                 => fail()
    }
  }

  test("test AnnoParam with name") {
    parse(s"(param:${TBool.prettyprint})", CoreParser.annoParam(_)) match {
      case Success(AnnoParam(Some("param"), TBool), _) =>
      case _                                           => fail()
    }
  }

  test("test AnnoParam without name") {
    parse(s"${TBool.prettyprint}", CoreParser.annoParam(_)) match {
      case Success(AnnoParam(None, TBool), _) =>
      case _                                  => fail()
    }
  }

  test("test Link core-links") {
    def checkLink(link: String, expected: Link): Unit = {
      parse(s"node.$link", CoreParser.link(_)) match {
        case Success(l, _) if l == expected =>
        case _                              => fail()
      }
    }
    checkLink("parent", ParentLink)
    checkLink("children", ChildrenLink)
    checkLink("next", NextLink)
    checkLink("prev", PreviousLink)
    checkLink("name", NamedLink(TNode("node"), "name"))
  }

  test("test Var") {
    parse("variable", CoreParser.varCoreExp(_)) match {
      case Success(Var("variable"), _) =>
      case _                           => fail()
    }
  }

  test("test Constant") {
    def check(lit: String, expected: Literal): Unit = {
      parse(lit, CoreParser.constantCoreExp(_)) match {
        case Success(Constant(literal), _) => if(literal != expected) {
          println(literal)
          fail()
        }
        case _                                                    => fail()
      }
    }
    check("1", IntLiteral(1))
    check("true", BooleanLiteral(true))
    check("1L", LongLiteral(1))
    check("unit", UnitLiteral)
    check("42d", DoubleLiteral(42d))
    check("\"hello world\"", StringLiteral("hello world"))
  }

  test("test Eq") {
    val expr = Eq(Var("x"), Var("y"))
    checkExp(expr)
  }

  test("test Neq") {
    val expr = Neq(Var("x"), Var("y"))
    checkExp(expr)
  }

  test("test Def") {
    val expr = Def(Var("x"))
    checkExp(expr)
  }

  test("test Undef") {
    val expr = Undef(Var("x"))
    checkExp(expr)
  }

  test("test InstanceOf") {
    val expr = InstanceOf(Var("x"), TBool)
    checkExp(expr)
  }

  test("test NotInstanceOf") {
    val expr = NotInstanceOf(Var("x"), TBool)
    checkExp(expr)
  }

  test("test CoreExp") {
    def test_helper(code: String, ast: CoreExp) {
      parse(code, CoreParser.coreExp(_)) match {
        case Failure(label, index, extra) => {}
        case Success(value, index)        => assert(value === ast)
      }
    }

    // Note: right to left input due to recusion
    test_helper(
      "(x == 5) instanceOf bool",
      InstanceOf(Eq(Var("x"), Constant(IntLiteral(5))), TBool)
    )
    test_helper(
      "x == 5 instanceOf bool",
      Eq(Var("x"), InstanceOf(Constant(IntLiteral(5)), TBool))
    )
    test_helper(
      "(x == (5 != y)) instanceOf int",
      InstanceOf(
        Eq(
          Var("x"),
          Neq(
            Constant(IntLiteral(5)),
            Var("y")
          )
        ),
        TInt
      )
    )
    test_helper(
      "def (x == 9) instanceOf long",
      Def(
        InstanceOf(
          Eq(
            Var("x"),
            Constant(IntLiteral(9))
          ),
          TLong
        )
      )
    )
    test_helper(
      "undef (q != def 17 instanceOf int) notInstanceOf string",
      Undef(
        NotInstanceOf(
          Neq(
            Var("q"),
            Def(
              InstanceOf(
                Constant(IntLiteral(17)),
                TInt
              )
            )
          ),
          TString
        )
      )
    )
  }

  private def checkExp(ex: Exp): Unit = {
    val input = ex.prettyprint("")
    println(input)
    parse(input, CoreParser.exp(_)) match {
      case Success(expr, _) => {
        println(expr, ex)
        assert(expr == ex)
      }
      case Failure(label, index, extra) => fail(s"${label}, ${index}, ${extra}")
    }
  }
}
