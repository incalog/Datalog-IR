package inca.frontend.parser

import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import inca.frontend.core.Core
import inca.frontend.core.Core._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import inca.backend.ir.GP.Pattern

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
        case Success(value, index)        => assert(value === t)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
      parse(s" ${t.prettyprint}", CoreParser.typeAnno(_)) match {
        case Success(value, index) => fail(s"$value, $index")
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
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
        case Success(value, index)        => assert(value === v)
      }
    def negative(v: String) =
      parse(v, CoreParser.visibility(_)) match {
        case Failure(label, index, extra) =>
        case Success(value, index)        => fail(s"$value, $index")
      }

    Seq("public", "   public", " public").map(positive(Public)(_))
    Seq("private", "   private", " private").map(positive(Private)(_))
    Seq("provate", "  prbplic", "   plsplapic", "bluplic").map(negative)
  }

  test("test TIterable") {
    parse("List[node]", CoreParser.tIterable(_)) match {
      case Success(value, index) =>
        value match {
          case TEnumeration(contained) => fail(s"Enum found instead of list.")
          case TList(contained)        => assert(contained === TAnyLinked)
        }
      case Failure(label, index, extra) => fail(s"$label, $index, $extra")
    }

    parse("List[apf3l]", CoreParser.tIterable(_)) match {
      case Success(value, index) =>
        value match {
          case TEnumeration(contained) => fail(s"Enum found instead of list.")
          case TList(contained)        => assert(contained === TNode("apf3l"))
        }
      case Failure(label, index, extra) => fail(s"$label, $index, $extra")
    }

    parse("Enum[node]", CoreParser.tIterable(_)) match {
      case Success(value, index) =>
        value match {
          case TEnumeration(contained) => assert(contained === TAnyLinked)
          case TList(contained)        => fail(s"List found instead of enum.")
        }
      case Failure(label, index, extra) => fail(s"$label, $index, $extra")
    }

    parse("Enum[br0t]", CoreParser.tIterable(_)) match {
      case Success(value, index) =>
        value match {
          case TEnumeration(contained) => assert(contained === TNode("br0t"))
          case TList(contained)        => fail(s"List found instead of enum.")
        }
      case Failure(label, index, extra) => fail(s"$label, $index, $extra")
    }

    parse("Enum[999]", CoreParser.tIterable(_)) match {
      case Success(value, index) => fail(s"$value, $index")
      case _: Failure            =>
    }

    parse("List[666]", CoreParser.tIterable(_)) match {
      case Success(value, index) => fail(s"$value, $index")
      case _: Failure            =>
    }
  }

  test("test IntLiteral") {
    def test_run(input: String, cmp: Int) =
      test_helper(CoreParser.intLiteral(_))(input, IntLiteral(cmp))

    test_run("0", 0)
    test_run("1", 1)
    test_run("12", 12)
    test_run("-1", -1)
    test_run("-12", -12)
  }

  test("test BooleanLiteral") {
    def test_helper(b: Boolean): Unit = {
      parse(b.toString, CoreParser.booleanLiteral(_)) match {
        case Success(BooleanLiteral(bool), _) => assert(bool === b)
        case Failure(label, index, extra)     => fail(s"$label, $index, $extra")
      }
    }
    test_helper(true)
    test_helper(false)
  }

  test("test LongLiteral") {
    def test_run(input: String, r: Long) =
      test_helper(CoreParser.longLiteral(_))(input, LongLiteral(r))
    test_run("1L", 1L)
    test_run("-1L", -1L)
    test_run("11L", 11L)
    test_run("-11L", -11L)
  }

  test("test DoubleLiteral") {
    def test_run(input: String, r: Double) =
      test_helper(CoreParser.doubleLiteral(_))(input, DoubleLiteral(r))
    test_run("1.", 1d)
    test_run("1d", 1d)
    test_run("1.0", 1d)
    test_run("1.0d", 1d)
    test_run("12.", 12d)
    test_run("12d", 12d)
    test_run("12.0", 12d)
    test_run("12.0d", 12d)
    test_run("0d", 0d)
    test_run("0.", 0d)
    test_run("0.0", 0d)
    test_run("0.0d", 0d)
    test_run("-1d", -1d)
    test_run("-1.", -1d)
    test_run("-1.0", -1d)
    test_run("-1.0d", -1d)
    test_run("-12d", -12d)
    test_run("-12.", -12d)
    test_run("-12.0", -12d)
    test_run("-12.0d", -12d)
    test_run("-1.1d", -1.1d)
    test_run("-1.1", -1.1d)
    test_run("-1.12", -1.12d)
    test_run("-1.12d", -1.12d)
    test_run("-12.1d", -12.1d)
    test_run("-12.1", -12.1d)
    test_run("-12.12", -12.12d)
    test_run("-12.12d", -12.12d)
  }

  test("test UnitLiteral") {
    parse("unit", CoreParser.unitLiteral(_)) match {
      case Success(UnitLiteral, _)      =>
      case Failure(label, index, extra) => fail(s"$label, $index, $extra")
    }
  }

  test("test Param") {
    parse(s"param:${TBool.prettyprint}", CoreParser.param(_)) match {
      case Success(Param("param", TBool), _) =>
      case Success(value, index)             => fail(s"$value, $index")
      case Failure(label, index, extra)      => fail(s"$label, $index, $extra")
    }
  }

  test("test AnnoParam with name") {
    def test_helper(t: TypeAnno) =
      parse(s"(param:${t.prettyprint})", CoreParser.annoParam(_)) match {
        case Success(AnnoParam(Some("param"), t), _) =>
        case Success(value, index)                   => fail(s"$value, $index")
        case Failure(label, index, extra)            => fail(s"$label, $index, $extra")
      }

    Seq(TBool, TDouble, TString, TInt, TLong, TAnyLinked, TNode("br0t")).map(test_helper)
  }

  test("test AnnoParam without name") {
    def test_helper(t: TypeAnno) =
      parse(TBool.prettyprint, CoreParser.annoParam(_)) match {
        case Success(AnnoParam(None, TBool), _) =>
        case Success(value, index)              => fail(s"$value, $index")
        case Failure(label, index, extra)       => fail(s"$label, $index, $extra")
      }

    Seq(TBool, TDouble, TString, TInt, TLong, TAnyLinked, TNode("br0t")).map(test_helper)
  }

  test("test Link core-links") {
    def checkLink(link: String, expected: Link): Unit = {
      parse(s"$link", CoreParser.link(TNode("node"))(_)) match {
        case Success(l, _) => assert(l === expected)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }
    checkLink("parent", ParentLink)
    checkLink("children", ChildrenLink)
    checkLink("next", NextLink)
    checkLink("prev", PreviousLink)
    checkLink("name", NamedLink(TNode("node"), "name"))
  }

  test("test Var") {
    // see: identifier
    parse("variable", CoreParser.varCoreExp(_)) match {
      case Success(value, _)            => assert(value === Var("variable"))
      case Failure(label, index, extra) => fail(s"$label, $index, $extra")
    }
  }

  test("test Constant") {
    def check(lit: String, expected: Literal): Unit = {
      parse(lit, CoreParser.constantCoreExp(_)) match {
        case Success(Constant(literal), _) =>
          if (literal != expected) {
            fail(s"${literal} != ${expected}")
          }
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
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

  test("test PathAccess ParentLink") {
    val expr = PathAccess(Var("xyz"), ParentLink)
    val input = expr.prettyprint("")
    parse(input, CoreParser.exp(_)) match {
      case Success(value, _)            =>
        print(value)
        assert(value == expr)
      case Failure(label, index, extra) => fail(s"$label, $index, $extra")
    }
  }

  test("test PathAccess NamedLink") {
    val expr = PathAccess(Var("test"), NamedLink(TNode("dummy"), "property"))
    val input = expr.prettyprint("")
    parse(input, CoreParser.exp(_)) match {
      case Success(value, _)            =>
        print(value)
        assert(value == expr)
      case Failure(label, index, extra) => fail(s"$label, $index, $extra")
    }
  }

  test("test CoreExp") {
    // @todo TODO: Add more test cases.
    def test_run = test_helper[CoreExp](CoreParser.coreExp(_))

    // Note: right to left input due to recusion
    test_run(
      "(x == 5) instanceOf bool",
      InstanceOf(Eq(Var("x"), Constant(IntLiteral(5))), TBool)
    )
    test_run(
      "x == 5 instanceOf bool",
      Eq(Var("x"), InstanceOf(Constant(IntLiteral(5)), TBool))
    )
    test_run(
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
    test_run(
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
    test_run(
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
    test_run(
      "\"String\" instanceOf string",
      InstanceOf(
        Constant(StringLiteral("String")),
        TString
      )
    )
    test_run(
      "9.7d notInstanceOf int",
      NotInstanceOf(Constant(DoubleLiteral(9.7)), TInt)
    )
    test_run(
      "8.7 instanceOf Float",
      InstanceOf(
        Constant(DoubleLiteral(8.7)),
        TNode("Float")
      )
    )
    test_run(
      "x instanceOf (bool,br0t)",
      InstanceOf(
        Var("x"),
        TTuple(Seq(TBool, TNode("br0t")))
      )
    )

    test_run(
      "undef x notInstanceOf (double)",
      Undef(
        NotInstanceOf(
          Var("x"),
          TTuple(Seq(TDouble))
        )
      )
    )
    test_run(
      "x instanceOf bool != x notInstanceOf double", // fails
      Neq(
        InstanceOf(
          Var("x"),
          TBool
        ),
        NotInstanceOf(
          Var("x"),
          TDouble
        )
      )
    )
    test_run(
      "x instanceOf (bool, br0t) != undef x notInstanceOf (double)", // fails
      Neq(
        InstanceOf(
          Var("x"),
          TTuple(Seq(TBool, TNode("br0t")))
        ),
        Undef(
          NotInstanceOf(
            Var("x"),
            TTuple(Seq(TDouble))
          )
        )
      )
    )
    test_run(
      "(x instanceOf (bool, br0t) != undef x) notInstanceOf (double)", // fails
      NotInstanceOf(
        Neq(
          InstanceOf(
            Var("x"),
            TTuple(
              Seq(TBool, TNode("br0t"))
            )
          ),
          Undef(Var("x"))
        ),
        TTuple(Seq(TDouble))
      )
    )
    // test_run(
    //   "x.parent",
    //   PathAccess(Var("x"), ParentLink)
    // )
    // test_run(
    //   "x.children",
    //   PathAccess(Var("x"), ChildrenLink)
    // )
    // test_run(
    //   "x.next",
    //   PathAccess(Var("x"), NextLink)
    // )
    // test_run(
    //   "x.prev",
    //   PathAccess(Var("x"), PreviousLink)
    // )
    // test_run(
    //   "x.size",
    //   PathAccess(Var("x"), SizeLink)
    // )
    test_run("foo()", Call("foo", Seq(), false))
    test_run("foo+()", Call("foo", Seq(), true))
    test_run("foo(x)", Call("foo", Seq(Var("x")), false))
    test_run("foo+(x)", Call("foo", Seq(Var("x")), true))
    test_run("foo(x, y)", Call("foo", Seq(Var("x"), Var("y")), false))
    test_run("foo+(x, y)", Call("foo", Seq(Var("x"), Var("y")), true))

    test_run("count foo()", Count(Call("foo", Seq(), false)))
    test_run("count foo+()", Count(Call("foo", Seq(), true)))
    test_run("count foo(x)", Count(Call("foo", Seq(Var("x")), false)))
    test_run("count foo+(x)", Count(Call("foo", Seq(Var("x")), true)))
    test_run("count foo(x, y)", Count(Call("foo", Seq(Var("x"), Var("y")), false)))
    test_run("count foo+(x, y)", Count(Call("foo", Seq(Var("x"), Var("y")), true)))

    test_run("(x, y)", Tuple(Seq(Var("x"), Var("y"))))
    test_run("(x, 5)", Tuple(Seq(Var("x"), Constant(IntLiteral(5)))))
    test_run(
      "(x, y, true)",
      Tuple(Seq(Var("x"), Var("y"), Constant(BooleanLiteral(true))))
    )

    test_run(
      "def (x, y) == (9, count foo+())",
      Def(
        Eq(
          Tuple(
            Seq(
              Var("x"),
              Var("y")
            )
          ),
          Tuple(
            Seq(
              Constant(IntLiteral(9)),
              Count(Call("foo", Seq.empty, true))
            )
          )
        )
      )
    )
  }

  test("test TTuple") {
    def test_run(input: String, cmp: TTuple) = test_helper[TTuple](CoreParser.tTuple(_))

    test_run("(int,string)", TTuple(Seq(TInt, TString)))
    test_run("(int, string)", TTuple(Seq(TInt, TString)))
    test_run("(int string)", TTuple(Seq(TInt, TString)))
    test_run(" ( int , string ) ", TTuple(Seq(TInt, TString)))
    test_run("(double)", TTuple(Seq(TDouble)))
    test_run("Unit", TTuple(Seq.empty))
  }

  test("test Statement") {
    checkStatement(Yield(Var("x")))
    checkStatement(Core.Fail)
    checkStatement(Values("x", TBool))
    checkStatement(Assign(Seq("x"), Var("y")))
    checkStatement(Assign(Seq("x", "y", "abc"), Var("zs")))
  }

  test("test Body") {
    def test_run(input: String, cmp: Body) = {
      test_helper[Body](CoreParser.body(_))(input, cmp)
      test_helper[Body](CoreParser.body(_))(input.replaceAll("\n", "\r\n"), cmp)
    }

    test_run(
      s"""{ 
          |    val x = 7
          |}""".stripMargin,
      Body(
        Seq(
          Assign(
            Seq(
              "x"
            ),
            Constant(IntLiteral(7))
          )
        )
      )
    )
    test_run(
      s"""{ 
          |    val (x, y) = 7
          |}""".stripMargin,
      Body(
        Seq(
          Assign(
            Seq(
              "x",
              "y"
            ),
            Constant(IntLiteral(7))
          )
        )
      )
    )
    test_run(
      s"""{ 
          |    val (x, y) = 7
          |  val q = 9
          |}""".stripMargin,
      Body(
        Seq(
          Assign(
            Seq(
              "x",
              "y"
            ),
            Constant(IntLiteral(7))
          ),
          Assign(
            Seq(
              "q"
            ),
            Constant(IntLiteral(9))
          )
        )
      )
    )
    test_run(
      s"""{ 
          |
          |
          |val (x, y) = 7
          |
          |  val q = 9
          |
          |}""".stripMargin,
      Body(
        Seq(
          Assign(
            Seq(
              "x",
              "y"
            ),
            Constant(IntLiteral(7))
          ),
          Assign(
            Seq(
              "q"
            ),
            Constant(IntLiteral(9))
          )
        )
      )
    )
    test_run(
      s"""{
          |val (x, y) = 7}""".stripMargin,
      Body(
        Seq(
          Assign(
            Seq(
              "x",
              "y"
            ),
            Constant(IntLiteral(7))
          )
        )
      )
    )
    test_run(
      s"""{
         |    assert x
         |}""".stripMargin,
      Body(
        Seq(
          Assert(Var("x"))
        )
      )
    )
    test_run(
      s"""{
         |    vals br0t <- (int, string)
         |}""".stripMargin,
      Body(
        Seq(
          Values("br0t", TTuple(Seq(TInt, TString)))
        )
      )
    )
  }

  test("test PatternFunction") {
    // @todo requires more tests
    def test_run(input: String, cmp: PatternFunction) = {
      test_helper[PatternFunction](CoreParser.patternFunction(_))(input, cmp)
      test_helper[PatternFunction](CoreParser.patternFunction(_))(
        input.replaceAll("\n", "\r\n"),
        cmp
      )
    }

    test_run(
      s"""def foo (bar: int) : unit = {
                |    assert x== 7 
                |} union { 
                |  val q = 9  
                |}""".stripMargin,
      PatternFunction(
        Option(Public),
        "foo",
        Seq(Param("bar", TInt)),
        Seq.empty,
        Seq(
          Body(
            Assert(Eq(Var("x"), Constant(IntLiteral(7))))
          ),
          Body(
            Assign(Seq("q"), Constant(IntLiteral(9)))
          )
        )
      )
    )

    test_run(
      s"""   def    foo    ( bar : int ) : unit =
                |{ 
                |    assert x  == 7 
                |}
                |union
                |{ 
                |  val q = 9  
                |}""".stripMargin,
      PatternFunction(
        Option(Public),
        "foo",
        Seq(Param("bar", TInt)),
        Seq.empty,
        Seq(
          Body(
            Assert(Eq(Var("x"), Constant(IntLiteral(7))))
          ),
          Body(
            Assign(Seq("q"), Constant(IntLiteral(9)))
          )
        )
      )
    )

    test_run(
      s"""private   def    foo    ( bar : int ) : (int) =
                |{ 
                |    assert x  == 7 
                |}
                |union
                |{ 
                |  val q = 9  
                |}""".stripMargin,
      PatternFunction(
        Option(Private),
        "foo",
        Seq(Param("bar", TInt)),
        Seq(AnnoParam(Option(null), TInt)),
        Seq(
          Body(
            Assert(Eq(Var("x"), Constant(IntLiteral(7))))
          ),
          Body(
            Assign(Seq("q"), Constant(IntLiteral(9)))
          )
        )
      )
    )

    test_run(
      s"""def foo(bar : int, foobar: (bool, (bool, string))) : ((string, bool)) = {
                |   val x = y
                |}""".stripMargin,
      PatternFunction(
        Option(Public),
        "foo",
        Seq(
          Param("bar", TInt),
          Param("foobar", TTuple(Seq(TBool, TTuple(Seq(TBool, TString)))))
        ),
        Seq(AnnoParam(Option(null), TTuple(Seq(TString, TBool)))),
        Seq(
          Body(
            Assign(Seq("x"), Var("y"))
          )
        )
      )
    )

    test_run(
      s"""def foo(bar : int, foobar: (bool, (bool, string))) : (value : (string, bool)) = {
                |   val x = y
                |}""".stripMargin,
      PatternFunction(
        Option(Public),
        "foo",
        Seq(
          Param("bar", TInt),
          Param("foobar", TTuple(Seq(TBool, TTuple(Seq(TBool, TString)))))
        ),
        Seq(AnnoParam(Option("value"), TTuple(Seq(TString, TBool)))),
        Seq(
          Body(
            Assign(Seq("x"), Var("y"))
          )
        )
      )
    )

    test_run(
      s"""def foo(bar : int, foobar: (bool, (bool, string))) : ((value : (string, bool)), bool) = {
                |   val x = y
                |}""".stripMargin,
      PatternFunction(
        Option(Public),
        "foo",
        Seq(
          Param("bar", TInt),
          Param("foobar", TTuple(Seq(TBool, TTuple(Seq(TBool, TString)))))
        ),
        Seq(
          AnnoParam(Option("value"), TTuple(Seq(TString, TBool))),
          AnnoParam(Option(null), TBool)
        ),
        Seq(
          Body(
            Assign(Seq("x"), Var("y"))
          )
        )
      )
    )
  }

  test("test Module") {
    def test_run = test_helper[Module](CoreParser.module(_))

    // Note the import keyword is optional
    test_run(
      s"""module my
                |math
                |import cuda_runtime
                |def foo(bar: bool): unit = {
                |  val x = y
                |}
                |def bar(foo: bool): unit = {
                |  val x = y
                |}""".stripMargin,
      Module(
        "my",
        Seq("math", "cuda_runtime"),
        Seq(
          PatternFunction(
            Option(Public),
            "foo",
            Seq(Param("bar", TBool)),
            Seq.empty,
            Seq(
              Body(
                Assign(Seq("x"), Var("y"))
              )
            )
          ),
          PatternFunction(
            Option(Public),
            "bar",
            Seq(Param("foo", TBool)),
            Seq.empty,
            Seq(
              Body(
                Assign(Seq("x"), Var("y"))
              )
            )
          )
        )
      )
    )

    test_run(
      s"""module my
                |
                |math
                |import cuda_runtime
                |
                |
                |def foo(bar: bool): unit = {
                |  val x = y
                |}
                |
                |""".stripMargin,
      Module(
        "my",
        Seq("math", "cuda_runtime"),
        Seq(
          PatternFunction(
            Option(Public),
            "foo",
            Seq(Param("bar", TBool)),
            Seq.empty,
            Seq(
              Body(
                Assign(Seq("x"), Var("y"))
              )
            )
          )
        )
      )
    )
    test_run(
      s"""module my
                |
                |import math
                |
                |
                |def foo(bar: bool): unit = {
                |  val x = y
                |}
                |
                |""".stripMargin,
      Module(
        "my",
        Seq("math"),
        Seq(
          PatternFunction(
            Option(Public),
            "foo",
            Seq(Param("bar", TBool)),
            Seq.empty,
            Seq(
              Body(
                Assign(Seq("x"), Var("y"))
              )
            )
          )
        )
      )
    )
    test_run(
      s"""module my
                |
                |def foo(bar: bool): unit = {
                |  val x = y
                |}
                |
                |""".stripMargin,
      Module(
        "my",
        Seq(),
        Seq(
          PatternFunction(
            Option(Public),
            "foo",
            Seq(Param("bar", TBool)),
            Seq.empty,
            Seq(
              Body(
                Assign(Seq("x"), Var("y"))
              )
            )
          )
        )
      )
    )
    test_run(
      s"""module my
                |""".stripMargin,
      Module(
        "my",
        Seq(),
        Seq()
      )
    )
  }

  private def test_helper[T](parser: P[_] => P[Any]) =
    (input: String, cmp: T) => {
      parse(input, parser) match {
        case Success(value, index)        => assert(cmp === value)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }

  private def checkStatement(stat: Statement): Unit = {
    val input = stat.prettyprint("  ")
    test_helper[Statement](CoreParser.statement(_))(input, stat)
  }

  private def checkExp(ex: Exp): Unit = {
    val input = ex.prettyprint("") // @todo prettyprint != AST @bug
    test_helper[Exp](CoreParser.exp(_))(input, ex)
  }
}
