package inca.frontend.parser

import inca.frontend.core.Core
import inca.frontend.core.Core._
import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import org.scalatest.Assertion

/**
  * Test class for the IncA core language parser @see CoreParser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class CoreParserTest extends AnyFunSuite {

  test("test TypeAnno") {
    def testTypeAnno(t: TypeAnno) = {
      testSuccess(CoreParser().typeAnno(_))(t.prettyprint, t)
//      testFailure(CoreParser().typeAnno(_))(s"   ${t.prettyprint}")
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
    ).map(testTypeAnno(_))
  }

  test("test Visibility") {
    def positive(v: Visibility)(t: String): Assertion = testSuccess(CoreParser().visibility(_))(t, v)
    def negative(v: String): Unit = testFailure(CoreParser().visibility(_))(v)

    positive(Public)("public")
    positive(Private)("private")
    Seq("provate", "  prbplic", "   plsplapic", "bluplic").map(negative)
  }

  test("test TIterable") {
    def testTIterable(inp : String, cmp : TIterable): Unit = {
      testSuccess(CoreParser().tIterable(_))(inp, cmp)
      testFailure(CoreParser().tIterable(_))(s"Q$inp")
    }

    testTIterable("List[Node]", TList(TAnyLinked))
    testTIterable("List[apf3l]", TList(TNode("apf3l")))
    testTIterable("Enum[Node]", TEnumeration(TAnyLinked))
    testTIterable("Enum[br0t]", TEnumeration(TNode("br0t")))
  }

  test("test IntLiteral") {
    def testIntLit(input: String, cmp: Int): Assertion =
      testSuccess(CoreParser().intLiteral(_))(input, IntLiteral(cmp))

    testIntLit("0", 0)
    testIntLit("1", 1)
    testIntLit("12", 12)
    testIntLit("-1", -1)
    testIntLit("-12", -12)
  }

  test("test BooleanLiteral") {
    def testBooleanLit(b: Boolean): Unit =
      testSuccess(CoreParser().booleanLiteral(_))(b.toString, BooleanLiteral(b))
    testBooleanLit(true)
    testBooleanLit(false)
  }

  test("test LongLiteral") {
    def testLongLit(input: String, r: Long): Assertion =
      testSuccess(CoreParser().longLiteral(_))(input, LongLiteral(r))
    testLongLit("1L", 1L)
    testLongLit("-1L", -1L)
    testLongLit("11L", 11L)
    testLongLit("-11L", -11L)
  }

  test("test DoubleLiteral") {
    def testDoubleLit(input: String, r: Double): Assertion =
      testSuccess(CoreParser().doubleLiteral(_))(input, DoubleLiteral(r))
    testDoubleLit("1.", 1d)
    testDoubleLit("1d", 1d)
    testDoubleLit("1.0", 1d)
    testDoubleLit("1.0d", 1d)
    testDoubleLit("12.", 12d)
    testDoubleLit("12d", 12d)
    testDoubleLit("12.0", 12d)
    testDoubleLit("12.0d", 12d)
    testDoubleLit("0d", 0d)
    testDoubleLit("0.", 0d)
    testDoubleLit("0.0", 0d)
    testDoubleLit("0.0d", 0d)
    testDoubleLit("-1d", -1d)
    testDoubleLit("-1.", -1d)
    testDoubleLit("-1.0", -1d)
    testDoubleLit("-1.0d", -1d)
    testDoubleLit("-12d", -12d)
    testDoubleLit("-12.", -12d)
    testDoubleLit("-12.0", -12d)
    testDoubleLit("-12.0d", -12d)
    testDoubleLit("-1.1d", -1.1d)
    testDoubleLit("-1.1", -1.1d)
    testDoubleLit("-1.12", -1.12d)
    testDoubleLit("-1.12d", -1.12d)
    testDoubleLit("-12.1d", -12.1d)
    testDoubleLit("-12.1", -12.1d)
    testDoubleLit("-12.12", -12.12d)
    testDoubleLit("-12.12d", -12.12d)
  }

  test("test UnitLiteral") {
    parse("unit", CoreParser().unitLiteral(_)) match {
      case Success(UnitLiteral, _)      =>
      case Failure(label, index, extra) => fail(s"$label, $index, $extra")
    }
  }

  test("test Param") {
    parse(s"param:${TBool.prettyprint}", CoreParser().param(_)) match {
      case Success(Param("param", TBool), _) =>
      case Success(value, index)             => fail(s"$value, $index")
      case Failure(label, index, extra)      => fail(s"$label, $index, $extra")
    }
  }

  test("test AnnoParam with name") {
    def testAnnoParam(t: TypeAnno): Assertion =
      testSuccess(CoreParser().annoParam(_))(
        s"(param:${t.prettyprint})",
        AnnoParam(Some("param"), t)
      )

    Seq(TBool, TDouble, TString, TInt, TLong, TAnyLinked, TNode("br0t")).map(testAnnoParam)
  }

  test("test AnnoParam without name") {
    def testAnnoParam(t: TypeAnno): Assertion =
      testSuccess(CoreParser().annoParam(_))(s"${t.prettyprint}", AnnoParam(None, t))

    Seq(TBool, TDouble, TString, TInt, TLong, TAnyLinked, TNode("br0t")).map(testAnnoParam)
  }

  test("test Link core-links") {
    val testLink = testSuccess(CoreParser().link(TNode("node"))(_))

    testLink("parent", ParentLink)
    testLink("children", ChildrenLink)
    testLink("next", NextLink)
    testLink("prev", PreviousLink)
    testLink("name", NamedLink("name"))
  }

  test("test Var") {
    def testVar(input: String) = testSuccess(CoreParser().varExp(_))(input, Var(input))

    Seq("variable", "br0t").map(testVar(_))
  }

  test("test Constant") {
    def testConstant(inp: String, cmp: Literal): Assertion =
      testSuccess(CoreParser().constantExp(_))(inp, Constant(cmp))

    testConstant("1", IntLiteral(1))
    testConstant("true", BooleanLiteral(true))
    testConstant("1L", LongLiteral(1))
    testConstant("unit", UnitLiteral)
    testConstant("42d", DoubleLiteral(42d))
    testConstant("\"hello world\"", StringLiteral("hello world"))
  }

  test("test Exp single") {
    def testExp(tree: Exp): Assertion =
      testSuccess(CoreParser().exp(_))(tree.prettyprint("  "), tree)

    testExp(Eq(Var("x"), Var("y")))
    testExp(Var("x"))
    testExp(Neq(Var("x"), Var("y")))
    testExp(Def(Var("x")))
    testExp(Undef(Var("x")))
    testExp(InstanceOf(Var("x"), TBool))
    testExp(NotInstanceOf(Var("x"), TBool))
    testExp(PathAccess(Var("xyz"), ParentLink))
    testExp(PathAccess(Var("test"), NamedLink("property")))
    testExp(
      Aggregate(
        DataOp(Some("br0t"), "with"),
        DataOp(Some("cheese"), "and"),
        None,
        Call("butter", Seq.empty, false)
      )
    )
  }

  test("test identifier") {
    def positive(v: String): Assertion =
      parse(v, CoreParser().identifier(_)) match {
        case Failure(label, index, extra) => fail()
        case Success(value, index)        => assert(value === v)
      }
    def negative(v: String): Unit =
      parse(v, CoreParser().identifier(_)) match {
        case Failure(label, index, extra) => {}
        case Success(value, index)        => fail()
      }

    Seq("kuch3n", "k3k53", "t33", "kl33", "br0t", "s0nn3nblum3").map(positive)
    Seq("3553n", "71nux", " ", "53h3n").map(negative)
  }

  test("test Eval params") {
    def testEvalParams(code: String, vars: Seq[Name], useBrackets: Boolean = false): Assertion = {
      val (open, close) = if(useBrackets) ("{", "}") else (("(", ")"))
      val input = s"eval$open $code $close"
      val parsed = parse(input, CoreParser().evalExp(_))
      parsed match {
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
        case Success(eval, _) =>
          val params = eval.params
          assert(
            params.size == vars.size && params.forall(vars.contains) && vars.forall(
              params.contains
            )
          )
      }
    }

    val code =
      """{
        | x.fun(-y - (z.point))
        | val exp = inf
        | val fun: Int => Int = n => code + 1
        | ten
        | }""".stripMargin
    for(b <- Set(true, false)) {
      testEvalParams(code, Seq("x", "y", "z", "inf", "code", "ten"), b)
    }

    val code1 =
      s"""{
         | val x = 5
         | val y = some
         | val abc = {
         |  val i = 10
         |  var obj = pen
         |  obj
         |  }
         | obj
         |}
         |""".stripMargin
    for(b <- Set(true, false)) {
      testEvalParams(code1, Seq("some", "pen", "obj"), b)
    }
  }

  test("test Exp combined") {
    // @todo Add more test cases.
    def testExp: (String, CoreExp) => Assertion = testSuccess[CoreExp](CoreParser().exp(_))

    // Note: right to left input due to recusion
    testExp(
      "(x == 5) instanceOf Boolean",
      InstanceOf(Eq(Var("x"), Constant(IntLiteral(5))), TBool)
    )
    testExp(
      "x == 5 instanceOf Boolean",
      Eq(Var("x"), InstanceOf(Constant(IntLiteral(5)), TBool))
    )
    testExp(
      "(x == (5 != y)) instanceOf Int",
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
    testExp(
      "def (x == 9) instanceOf Long",
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
    testExp(
      "undef (q != def 17 instanceOf Int) notInstanceOf String",
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
    testExp(
      "\"String\" instanceOf String",
      InstanceOf(
        Constant(StringLiteral("String")),
        TString
      )
    )
    testExp(
      "9.7d notInstanceOf Int",
      NotInstanceOf(Constant(DoubleLiteral(9.7)), TInt)
    )
    testExp(
      "8.7 instanceOf Float",
      InstanceOf(
        Constant(DoubleLiteral(8.7)),
        TNode("Float")
      )
    )
    testExp(
      "x instanceOf (Boolean,br0t)",
      InstanceOf(
        Var("x"),
        TTuple(Seq(TBool, TNode("br0t")))
      )
    )

    testExp(
      "undef x notInstanceOf (Double)",
      Undef(
        NotInstanceOf(
          Var("x"),
          TTuple(Seq(TDouble))
        )
      )
    )
    testExp(
      "x instanceOf Boolean != x notInstanceOf Double",
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
    testExp(
      "x instanceOf (Boolean, br0t) != undef x notInstanceOf (Double)",
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
    testExp(
      "(x instanceOf (Boolean, br0t) != undef x) notInstanceOf (Double)",
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
    testExp(
      "x.parent",
      PathAccess(Var("x"), ParentLink)
    )
    testExp(
      "x.children",
      PathAccess(Var("x"), ChildrenLink)
    )
    testExp(
      "x.next",
      PathAccess(Var("x"), NextLink)
    )
    testExp(
      "x.prev",
      PathAccess(Var("x"), PreviousLink)
    )
    testExp(
      "x.size",
      PathAccess(Var("x"), SizeLink)
    )
    testExp("foo()", Call("foo", Seq(), false))
    testExp("foo+()", Call("foo", Seq(), true))
    testExp("foo(x)", Call("foo", Seq(Var("x")), false))
    testExp("foo+(x)", Call("foo", Seq(Var("x")), true))
    testExp("foo(x, y)", Call("foo", Seq(Var("x"), Var("y")), false))
    testExp("foo+(x, y)", Call("foo", Seq(Var("x"), Var("y")), true))

    testExp("count foo()", Count(Call("foo", Seq(), false)))
    testExp("count foo+()", Count(Call("foo", Seq(), true)))
    testExp("count foo(x)", Count(Call("foo", Seq(Var("x")), false)))
    testExp("count foo+(x)", Count(Call("foo", Seq(Var("x")), true)))
    testExp("count foo(x, y)", Count(Call("foo", Seq(Var("x"), Var("y")), false)))
    testExp("count foo+(x, y)", Count(Call("foo", Seq(Var("x"), Var("y")), true)))

    testExp("(x, y)", Tuple(Seq(Var("x"), Var("y"))))
    testExp("(x, 5)", Tuple(Seq(Var("x"), Constant(IntLiteral(5)))))
    testExp(
      "(x, y, true)",
      Tuple(Seq(Var("x"), Var("y"), Constant(BooleanLiteral(true))))
    )

    testExp(
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
    def testTTuple(input: String, cmp: TTuple): (String, TTuple) => Assertion = testSuccess[TTuple](CoreParser().tTuple(_))

    testTTuple("(Int,String)", TTuple(Seq(TInt, TString)))
    testTTuple("(Int, String)", TTuple(Seq(TInt, TString)))
    testTTuple("(Int String)", TTuple(Seq(TInt, TString)))
    testTTuple(" ( Int , String ) ", TTuple(Seq(TInt, TString)))
    testTTuple("(Double)", TTuple(Seq(TDouble)))
    testTTuple("Unit", TTuple(Seq.empty))
  }

  test("test Statement") {
    def testStatement(tree: Statement): Assertion =
      testSuccess(CoreParser().statement(_))(tree.prettyprint(""), tree)

    testStatement(Yield(Var("x")))
    testStatement(Core.Fail)
    testStatement(Values("x", TBool))
    testStatement(Assign(Seq("x"), Var("y")))
    testStatement(Assign(Seq("x", "y", "abc"), Var("zs")))
  }

  test("test Body") {
    def testBody(input: String, cmp: Body): Assertion = {
      testSuccess[Body](CoreParser().body(_))(input, cmp)
      testSuccess[Body](CoreParser().body(_))(input.replaceAll("\n", "\r\n"), cmp)
    }

    testBody(
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
    testBody(
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
    testBody(
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
    testBody(
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
    testBody(
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
    testBody(
      s"""{
         |    assert x
         |}""".stripMargin,
      Body(
        Seq(
          Assert(Var("x"))
        )
      )
    )
    testBody(
      s"""{
         |    vals br0t <- (Int, String)
         |}""".stripMargin,
      Body(
        Seq(
          Values("br0t", TTuple(Seq(TInt, TString)))
        )
      )
    )
  }

  test("test PatternFunction") {
    // @todo Add more test cases.
    def testPatternFunction(input: String, cmp: PatternFunction): Assertion = {
      testSuccess[PatternFunction](CoreParser().patternFunction(_))(input, cmp)
      testSuccess[PatternFunction](CoreParser().patternFunction(_))(
        input.replaceAll("\n", "\r\n"),
        cmp
      )
    }

    testPatternFunction(
      s"""def foo (bar: Int) : Unit = {
                |    assert x== 7 
                |} union { 
                |  val q = 9  
                |}""".stripMargin,
      PatternFunction(
        None,
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

    testPatternFunction(
      s"""def    foo    ( bar : Int ) : Unit =
                |{ 
                |    assert x  == 7 
                |}
                |union
                |{ 
                |  val q = 9  
                |}""".stripMargin,
      PatternFunction(
        None,
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

    testPatternFunction(
      s"""private   def    foo    ( bar : Int ) : (Int) =
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

    testPatternFunction(
      s"""def foo(bar : Int, foobar: (Boolean, (Boolean, String))) : ((String, Boolean)) = {
                |   val x = y
                |}""".stripMargin,
      PatternFunction(
        None,
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

    testPatternFunction(
      s"""public def foo(bar : Int, foobar: (Boolean, (Boolean, String))) : (value : (String, Boolean)) = {
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

    testPatternFunction(
      s"""def foo(bar : Int, foobar: (Boolean, (Boolean, String))) : ((value : (String, Boolean)), Boolean) = {
                |   val x = y
                |}""".stripMargin,
      PatternFunction(
        None,
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
    def testModule = testSuccess[Module](CoreParser().module(_))

    // Note the import keyword is optional
    testModule(
      s"""module my
                |import math
                |import cuda_runtime
                |public def foo(bar: Boolean): Unit = {
                |  val x = y
                |}
                |def bar(foo: Boolean): Unit = {
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
            None,
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

    testModule(
      s"""module my
                |
                |import cuda_runtime
                |
                |
                |def foo(bar: Boolean): Unit = {
                |  val x = y
                |}
                |
                |""".stripMargin,
      Module(
        "my",
        Seq("cuda_runtime"),
        Seq(
          PatternFunction(
            None,
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
    testModule(
      s"""module my
                |
                |import math
                |
                |
                |def foo(bar: Boolean): Unit = {
                |  val x = y
                |}
                |
                |""".stripMargin,
      Module(
        "my",
        Seq("math"),
        Seq(
          PatternFunction(
            None,
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
    testModule(
      s"""module my
                |
                |def foo(bar: Boolean): Unit = {
                |  val x = y
                |}
                |
                |""".stripMargin,
      Module(
        "my",
        Seq(),
        Seq(
          PatternFunction(
            None,
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
    testModule(
      s"""module my
                |""".stripMargin,
      Module(
        "my",
        Seq(),
        Seq()
      )
    )
  }

  test("test DataOp") {
    // @todo Add more tests.
    val testDataOp: (String, Any) => Assertion = testSuccess(CoreParser().dataOp(_))

    testDataOp("br0t.br0t", DataOp(Some("br0t"), "br0t"))
    testDataOp("br0t", DataOp(None, "br0t"))
  }

  test("test Eval") {
    def testEval(input: String, vars: Set[String], cmp_code : String): Assertion =
      parse(input, CoreParser().evalExp(_)) match {
        case Success(Eval(ss, code), index) => {
          assert(cmp_code === code.syntax)
          assert((Set.empty[String] ++ ss) === vars)
        }
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }

    testEval("eval(x + 2)", Set("x"), "x + 2")
    testEval("eval(x + y + p)", Set("x", "y", "p"), "x + y + p")
    // testEval(
    //   s"""|eval( x match {
    //              |   case 1 => 2 
    //              |   case 2 => 4 
    //              |   case 3 => y
    //              |   case _ => 42
    //              |})""".stripMargin,
    //   Set("x", "y"),
    //   s"""| x match {
    //       |   case 1 => 2 
    //       |   case 2 => 4 
    //       |   case 3 => y
    //       |   case _ => 42
    //       |}""".stripMargin
    // )
  }

  private def testSuccess[T](parser: P[_] => P[Any]): (String, T) => Assertion =
    (input: String, cmp: T) => {
      parse(input, parser) match {
        case Success(value, index)        => assert(value === cmp)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }

  private def testFailure[T](parser: P[_] => P[Any]): String => Unit =
    (input: String) => {
      parse(input, parser) match {
        case Success(value, index)        => fail(s"$value, $index")
        case Failure(label, index, extra) =>
      }
    }
}
