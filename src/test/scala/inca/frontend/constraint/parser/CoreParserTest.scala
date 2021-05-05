package inca.frontend.constraint.parser

import fastparse.Parsed.{Failure, Success}
import fastparse._
import inca.frontend.constraint.core
import inca.runtime.context.DataModel
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

/**
  * Test class for the IncA core language parser @see Parser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class CoreParserTest extends AnyFunSuite {

  val parser = new CoreParser {}
  import inca.frontend.constraint.core.tree._
  
  test("test Type") {
    def testType(t: Type) = {
      testSuccess(parser.typeAnno(_))(t.prettyprint, t)
//      testFailure(parser.typeAnno(_))(s"   ${t.prettyprint}")
    }

    Seq(
      TAny,
      TLiteral.Int,
      TLiteral.Bool,
      TLiteral.Double,
      TLiteral.Long,
      TLiteral.String,
      TAnyLinked,
      TNode("t0mat3"),
      TNode("apf3l"),
      TNode("k1r5ch3")
    ).map(testType(_))
  }

  test("test Visibility") {
    def positive(v: Visibility)(t: String): Assertion = testSuccess(parser.visibility(_))(t, v)
    def negative(v: String): Unit = testFailure(parser.visibility(_))(v)

    positive(Private)("private")
    Seq("public", "provate", "  prbplic", "   plsplapic", "bluplic").map(negative)
  }

  test("test TIterable") {
    def testTIterable(inp : String, cmp : TIterable): Unit = {
      testSuccess(parser.tIterable(_))(inp, cmp)
      testFailure(parser.tIterable(_))(s"Q$inp")
    }

    testTIterable("List[AnyNode]", TList(TAnyLinked))
    testTIterable("List[apf3l]", TList(TNode("apf3l")))
    testTIterable("Enum[AnyNode]", TEnumeration(TAnyLinked))
    testTIterable("Enum[br0t]", TEnumeration(TNode("br0t")))
  }

  test("test IntLiteral") {
    def testIntLit(input: String, cmp: Int): Assertion =
      testSuccess(parser.numericLiteral(_))(input, IntLiteral(cmp))

    testIntLit("0", 0)
    testIntLit("1", 1)
    testIntLit("12", 12)
    testIntLit("-1", -1)
    testIntLit("-12", -12)
  }

  test("test BooleanLiteral") {
    def testBooleanLit(b: Boolean): Unit =
      testSuccess(parser.booleanLiteral(_))(b.toString, BooleanLiteral(b))
    testBooleanLit(true)
    testBooleanLit(false)
  }

  test("test LongLiteral") {
    def testLongLit(input: String, r: Long): Assertion =
      testSuccess(parser.numericLiteral(_))(input, LongLiteral(r))
    testLongLit("1L", 1L)
    testLongLit("-1L", -1L)
    testLongLit("11L", 11l)
    testLongLit("-11L", -11L)
    testLongLit("-111111111111111L", -111111111111111L)
  }

  test("test DoubleLiteral") {
    def testDoubleLit(input: String, r: Double): Assertion =
      testSuccess(parser.numericLiteral(_))(input, DoubleLiteral(r))
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
    parse("unit", parser.unitLiteral(_)) match {
      case Success(UnitLiteral, _)      =>
      case Failure(label, index, extra) => fail(s"$label, $index, $extra")
    }
  }

  test("test Param") {
    parse(s"param:Boolean", parser.param(_)) match {
      case Success(Param(Name("param"), TLiteral.Bool), _) =>
      case Success(value, index)             => fail(s"$value, $index")
      case Failure(label, index, extra)      => fail(s"$label, $index, $extra")
    }
  }


  test("test Link core-links") {
    val testLink = testSuccess(parser.link(_))

    testLink("parent", ParentLink)
    testLink("children", ChildrenLink)
    testLink("next", NextLink)
    testLink("prev", PreviousLink)
    testLink("name", NamedLink(Name("name")))
  }

  test("test Var") {
    def testVarSuccess(input: String) = testSuccess(parser.varExp(_))(input, Var(Name(input)))
    def testVarFailure(input: String) = testFailure(parser.varExp(_))

    Seq("variable", "br0t").map(testVarSuccess)
    testVarFailure("_")
  }

  test("test Wildcard") {
    def testWildcard(input: String) = testSuccess(parser.wildcardExp(_))(input, Wildcard)

    testWildcard("_")
  }

  test("test Constant") {
    def testConstant(inp: String, cmp: Literal): Assertion =
      testSuccess(parser.constantExp(_))(inp, Constant(cmp))

    testConstant("1", IntLiteral(1))
    testConstant("true", BooleanLiteral(true))
    testConstant("1L", LongLiteral(1))
    testConstant("unit", UnitLiteral)
    testConstant("42d", DoubleLiteral(42d))
    testConstant("\"hello world\"", StringLiteral("hello world"))
  }

  test("test Exp single") {
    def testExp(tree: Expression): Assertion =
      testSuccess(parser.exp(_))(tree.prettyprint("  "), tree)

    testExp(Eq(Var("x"), Var("y")))
    testExp(Var("x"))
    testExp(Neq(Var("x"), Var("y")))
    testExp(Def(Var("x")))
    testExp(Undef(Var("x")))
    testExp(InstanceOf(Var("x"), TLiteral.Bool))
    testExp(NotInstanceOf(Var("x"), TLiteral.Bool))
    testExp(PathAccess(Var("xyz"), ParentLink))
    testExp(PathAccess(Var("test"), NamedLink("property")))
    testExp(
      Aggregate(
        Var("and"),
        Seq(Body(Seq(Yield(Constant(BooleanLiteral(true))))),
            Body(Seq(Yield(Constant(BooleanLiteral(false))))))
      )
    )
  }

  test("test identifier") {
    def positive(v: String): Assertion =
      parse(v, parser.identifier(_)) match {
        case Failure(label, index, extra) => fail()
        case Success(value, index)        => assert(value === Name(v))
      }
    def negative(v: String): Unit =
      parse(v, parser.identifier(_)) match {
        case Failure(label, index, extra) => {}
        case Success(value, index)        => fail()
      }

    Seq("kuch3n", "k3k53", "t33", "kl33", "br0t", "s0nn3nblum3").map(positive)
    Seq("3553n", "71nux", " ", "53h3n").map(negative)
  }

//  test("test Eval params") {
//    def testEvalParams(code: String, vars: Seq[Name], useBrackets: Boolean = false): Assertion = {
//      val input = s"`$code`"
//      val parsed = parse(input, parser.evalExp(_))
//      parsed match {
//        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
//        case Success(eval, _) =>
//          val params = eval.params
//          val paramNames = params.map(_.name)
//          assert(
//            params.size == vars.size && paramNames.toSet == vars.toSet
//          )
//      }
//    }
//
//    val code =
//      """{
//        | x.fun(-y - (z.point))
//        | val exp = inf
//        | val fun: Int => Int = n => code + 1
//        | ten
//        | }""".stripMargin
//    for(b <- Set(true, false)) {
//      testEvalParams(code, Seq("x", "y", "z", "inf", "code", "ten").map(Name.apply), b)
//    }
//
//    val code1 =
//      s"""{
//         | val x = 5
//         | val y = some
//         | val abc = {
//         |  val i = 10
//         |  var obj = pen
//         |  obj
//         |  }
//         | obj
//         |}
//         |""".stripMargin
//    for(b <- Set(true, false)) {
//      testEvalParams(code1, Seq("some", "pen", "obj").map(Name.apply), b)
//    }
//  }

  test("test Exp combined") {
    def testExp: (String, CoreExpression) => Assertion = testSuccess[CoreExpression](parser.exp(_))
    def testExpFail: String => Unit = testFailure(parser.exp(_))

    // Note: right to left input due to recusion
    testExp(
      "(x == 5).isInstanceOf[Boolean]",
      InstanceOf(Eq(Var("x"), Constant(IntLiteral(5))), TLiteral.Bool)
    )
    testExpFail(
      "x == 5.isInstanceOf[Boolean]"
    )
    testExp(
      "(x == (5 != y)).isInstanceOf[Int]",
      InstanceOf(
        Eq(
          Var("x"),
          Neq(
            Constant(IntLiteral(5)),
            Var("y")
          )
        ),
        TLiteral.Int
      )
    )
    testExp(
      "def (x == 9).isInstanceOf[Long]",
      Def(
        InstanceOf(
          Eq(
            Var("x"),
            Constant(IntLiteral(9))
          ),
          TLiteral.Long
        )
      )
    )
    testExpFail (
      "undef (q != def 17.isInstanceOf[Int]).notInstanceOf[String]"
    )
    testExp(
      "\"String\".isInstanceOf[String]",
      InstanceOf(
        Constant(StringLiteral("String")),
        TLiteral.String
      )
    )
    testExp(
      "9.7d.notInstanceOf[Int]",
      NotInstanceOf(Constant(DoubleLiteral(9.7)), TLiteral.Int)
    )
    testExp(
      "8.7.isInstanceOf[Float]",
      InstanceOf(
        Constant(DoubleLiteral(8.7)),
        TNode("Float")
      )
    )
    testExp(
      "x.isInstanceOf[(Boolean,br0t)]",
      InstanceOf(
        Var("x"),
        TTuple(Seq(TLiteral.Bool, TNode("br0t")))
      )
    )

    testExp(
      "undef x.notInstanceOf[(Double)]",
      Undef(
        NotInstanceOf(
          Var("x"),
          TLiteral.Double
        )
      )
    )
    testExp(
      "x.isInstanceOf[Boolean] != x.notInstanceOf[Double]",
      Neq(
        InstanceOf(
          Var("x"),
          TLiteral.Bool
        ),
        NotInstanceOf(
          Var("x"),
          TLiteral.Double
        )
      )
    )
    testExpFail(
      "x.isInstanceOf[(Boolean, br0t)] != undef x.notInstanceOf[Double]"
    )
    testExpFail(
      "(x.isInstanceOf[(Boolean, br0t)] != undef x).notInstanceOf[Double]"
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
    testExp("foo()", Call(Name("foo"), Seq(), false))
    testExp("foo+()", Call(Name("foo"), Seq(), true))
    testExp("foo(x)", Call(Name("foo"), Seq(Var("x")), false))
    testExp("foo+(x)", Call(Name("foo"), Seq(Var("x")), true))
    testExp("foo(x, y)", Call(Name("foo"), Seq(Var("x"), Var("y")), false))
    testExp("foo+(x, y)", Call(Name("foo"), Seq(Var("x"), Var("y")), true))

    testExp("count foo()", Count(Call(Name("foo"), Seq(), false)))
    testExp("count foo+()", Count(Call(Name("foo"), Seq(), true)))
    testExp("count foo(x)", Count(Call(Name("foo"), Seq(Var("x")), false)))
    testExp("count foo+(x)", Count(Call(Name("foo"), Seq(Var("x")), true)))
    testExp("count foo(x, y)", Count(Call(Name("foo"), Seq(Var("x"), Var("y")), false)))
    testExp("count foo+(x, y)", Count(Call(Name("foo"), Seq(Var("x"), Var("y")), true)))

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
              Count(Call(Name("foo"), Seq.empty, true))
            )
          )
        )
      )
    )
  }

  test("test TTuple") {
    def testTTuple(input: String, cmp: Type): (String, TTuple) => Assertion = testSuccess[TTuple](parser.tTuple(_))

    testTTuple("(Int,String)", TTuple(Seq(TLiteral.Int, TLiteral.String)))
    testTTuple("(Int, String)", TTuple(Seq(TLiteral.Int, TLiteral.String)))
    testTTuple("(Int String)", TTuple(Seq(TLiteral.Int, TLiteral.String)))
    testTTuple(" ( Int , String ) ", TTuple(Seq(TLiteral.Int, TLiteral.String)))
    testTTuple("(Double)", TLiteral.Double)
    testTTuple("Unit", TTuple(Seq.empty))
  }

  test("test Statement") {
    def testStatement(tree: Statement): Assertion =
      testSuccess(parser.statement(_))(tree.prettyprint(""), tree)

    testStatement(Yield(Var("x")))
    testStatement(FailStatement)
    testStatement(Values(Name("x"), TLiteral.Bool))
    testStatement(Assign(Seq(Name("x")), Var("y")))
    testStatement(Assign(Seq(Name("x"), Name("y"), Name("abc")), Var("zs")))
  }

  test("test Body") {
    def testBody(input: String, cmp: Body): Assertion = {
      testSuccess[Body](parser.body(_))(input, cmp)
      testSuccess[Body](parser.body(_))(input.replaceAll("\n", "\r\n"), cmp)
    }

    testBody(
      s"""{ 
          |    val x = 7
          |}""".stripMargin,
      Body(
        Seq(
          Assign(
            Seq(
              Name("x")
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
              Name("x"),
              Name("y")
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
              Name("x"),
              Name("y")
            ),
            Constant(IntLiteral(7))
          ),
          Assign(
            Seq(
              Name("q")
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
              Name("x"),
              Name("y")
            ),
            Constant(IntLiteral(7))
          ),
          Assign(
            Seq(
              Name("q")
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
              Name("x"),
              Name("y")
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
         |    assert x.isInstanceOf[Foo]
         |}""".stripMargin,
      Body(
        Seq(
          Assert(InstanceOf(Var("x"), TNode("Foo")))
        )
      )
    )
    testBody(
      s"""{
         |    vals br0t <- (Int, String)
         |}""".stripMargin,
      Body(
        Seq(
          Values(Name("br0t"), TTuple(Seq(TLiteral.Int, TLiteral.String)))
        )
      )
    )
  }

  test("test PatternFunction") {
    def testPatternFunction(input: String, cmp: PatternFunction): Assertion = {
      testSuccess[PatternFunction](parser.patternFunction(_))(input, cmp)
      testSuccess[PatternFunction](parser.patternFunction(_))(
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
        Name("foo"),
        Seq(Param(Name("bar"), TLiteral.Int)),
        TUnit,
        Seq(
          Body(
            Assert(Eq(Var("x"), Constant(IntLiteral(7))))
          ),
          Body(
            Assign(Seq(Name("q")), Constant(IntLiteral(9)))
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
        Name("foo"),
        Seq(Param(Name("bar"), TLiteral.Int)),
        TUnit,
        Seq(
          Body(
            Assert(Eq(Var("x"), Constant(IntLiteral(7))))
          ),
          Body(
            Assign(Seq(Name("q")), Constant(IntLiteral(9)))
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
        Name("foo"),
        Seq(Param(Name("bar"), TLiteral.Int)),
        TLiteral.Int,
        Seq(
          Body(
            Assert(Eq(Var("x"), Constant(IntLiteral(7))))
          ),
          Body(
            Assign(Seq(Name("q")), Constant(IntLiteral(9)))
          )
        )
      )
    )

    testPatternFunction(
      s"""def foo(bar : Int, foobar: (Boolean, (Boolean, String))) : (String, Boolean) = {
                |   val x = y
                |}""".stripMargin,
      PatternFunction(
        None,
        Name("foo"),
        Seq(
          Param(Name("bar"), TLiteral.Int),
          Param(Name("foobar"), TTuple(Seq(TLiteral.Bool, TTuple(Seq(TLiteral.Bool, TLiteral.String)))))
        ),
        TTuple(Seq(TLiteral.String, TLiteral.Bool)),
        Seq(
          Body(
            Assign(Seq(Name("x")), Var("y"))
          )
        )
      )
    )

    testPatternFunction(
      s"""def foo(bar : Int, foobar: (Boolean, (Boolean, String))) : (String, Boolean) = {
                |   val x = y
                |}""".stripMargin,
      PatternFunction(
        None,
        Name("foo"),
        Seq(
          Param(Name("bar"), TLiteral.Int),
          Param(Name("foobar"), TTuple(Seq(TLiteral.Bool, TTuple(Seq(TLiteral.Bool, TLiteral.String)))))
        ),
        TTuple(Seq(TLiteral.String, TLiteral.Bool)),
        Seq(
          Body(
            Assign(Seq(Name("x")), Var("y"))
          )
        )
      )
    )

    testPatternFunction(
      s"""def foo(bar : Int, foobar: (Boolean, (Boolean, String))) : ((String, Boolean), Boolean) = {
                |   val x = y
                |}""".stripMargin,
      PatternFunction(
        None,
        Name("foo"),
        Seq(
          Param(Name("bar"), TLiteral.Int),
          Param(Name("foobar"), TTuple(Seq(TLiteral.Bool, TTuple(Seq(TLiteral.Bool, TLiteral.String)))))
        ),
        TTuple(Seq(
          TTuple(Seq(TLiteral.String, TLiteral.Bool)),
          TLiteral.Bool)
        ),
        Seq(
          Body(
            Assign(Seq(Name("x")), Var("y"))
          )
        )
      )
    )
  }

  test("test Module") {
    def testModule = testSuccess[Module](parser.module(_))

    testModule(
      s"""module my
         |datamodel inca.analyzedLangs.Exp.dataModel
         |import math
         |import cuda_runtime
         |def foo(bar: Boolean): Unit = {
         |  val x = y
         |}
         |def bar(foo: Boolean): Unit = {
         |  val x = y
         |}""".stripMargin,
      Module(
        Name("my"),
        Seq(NativeDataModel("inca.analyzedLangs.Exp.dataModel")),
        Seq(Import(Name("math")), Import(Name("cuda_runtime"))),
        Seq(),
        Seq(
          PatternFunction(
            None,
            Name("foo"),
            Seq(Param(Name("bar"), TLiteral.Bool)),
            TUnit,
            Seq(
              Body(
                Assign(Seq(Name("x")), Var("y"))
              )
            )
          ),
          PatternFunction(
            None,
            Name("bar"),
            Seq(Param(Name("foo"), TLiteral.Bool)),
            TUnit,
            Seq(
              Body(
                Assign(Seq(Name("x")), Var("y"))
              )
            )
          )
        )
      )
    )

    testModule(
      s"""module my
         |import cuda_runtime
         |
         |
         |def foo(bar: Boolean): Unit = {
         |  val x = y
         |}
         |
         |""".stripMargin,
      Module(
        Name("my"),
        Seq(),
        Seq(Import(Name("cuda_runtime"))),
        Seq(),
        Seq(
          PatternFunction(
            None,
            Name("foo"),
            Seq(Param(Name("bar"), TLiteral.Bool)),
            TUnit,
            Seq(
              Body(
                Assign(Seq(Name("x")), Var("y"))
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
        Name("my"),
        Seq(),
        Seq(Import(Name("math"))),
        Seq(),
        Seq(
          PatternFunction(
            None,
            Name("foo"),
            Seq(Param(Name("bar"), TLiteral.Bool)),
            TUnit,
            Seq(
              Body(
                Assign(Seq(Name("x")), Var("y"))
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
        Name("my"),
        Seq(),
        Seq(),
        Seq(),
        Seq(
          PatternFunction(
            None,
            Name("foo"),
            Seq(Param(Name("bar"), TLiteral.Bool)),
            TUnit,
            Seq(
              Body(
                Assign(Seq(Name("x")), Var("y"))
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
        Name("my"),
        Seq(),
        Seq(),
        Seq(),
        Seq()
      )
    )
  }

  test("test Module with val defs") {
    def testModule = testSuccess[Module](parser.module(_))

    testModule(
      s"""module my
         |val x = 1
         |""".stripMargin,
      Module(
        Name("my"),
        Seq(),
        Seq(),
        Seq(),
        Seq(ValDef(None, Name("x"), None, Constant(IntLiteral(1))))
      )
    )
    testModule(
      s"""module my
         |val x: `Int` = 1
         |""".stripMargin,
      Module(
        Name("my"),
        Seq(),
        Seq(),
        Seq(),
        Seq(ValDef(None, Name("x"), Some(TScalaInt), Constant(IntLiteral(1))))
      )
    )
    testModule(
      s"""module my
         |val x: `Int` = 1
         |val y = x
         |""".stripMargin,
      Module(
        Name("my"),
        Seq(),
        Seq(),
        Seq(),
        Seq(
          ValDef(None, Name("x"), Some(TScalaInt), Constant(IntLiteral(1))),
          ValDef(None, Name("y"), None, Var(Name("x")))
        )
      )
    )
  }

  test("test Eval") {
    def testEval(input: String, cmp_code : String): Assertion =
      parse(input, parser.evalExp(_)) match {
        case Success(Eval(code), index) => {
          assert(cmp_code === code.syntax)
//          assert(ss.map(_.name).toSet === vars)
        }
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }

    testEval("`x + 2`", "x + 2")
    testEval("`x + y + p`", "x + y + p")
    testEval("`if (x) { import inca._;inca.one } else { 1 }`", "if (x) { import inca._;inca.one } else { 1 }")
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

  test("test scala types") {
    val testTypeSuccess = testSuccess(parser.typeAnno(_))
    testTypeSuccess("`Int`", TScalaInt)
    testTypeSuccess("`Boolean`", TScalaBoolean)
    testTypeSuccess("`String`", TScalaString)
    testTypeSuccess("`Any`", TScala("Any"))
    testTypeSuccess("`inca.util.Meta`", TScala("inca.util.Meta"))
  }

  test("test Cast") {
    val testCastSuccess = testSuccess(parser.exp(_))

    testCastSuccess("x:Int", Cast(Var("x"), TLiteral.Int))
    testCastSuccess("x :Int", Cast(Var("x"), TLiteral.Int))
    testCastSuccess("x : Int", Cast(Var("x"), TLiteral.Int))
    testCastSuccess("x:Int:Int", Cast(Cast(Var("x"), TLiteral.Int), TLiteral.Int))
  }

  private def testSuccess[T](parser: P[_] => P[Any]): (String, T) => Assertion =
    (input: String, cmp: T) => {
      parse(input, parser) match {
        case Success(value, index)        =>
          println(value)
          assert(value === cmp)
          assertResult(input.length)(index)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }

  private def testFailure[T](parser: P[_] => P[Any]): String => Unit =
    (input: String) => {
      parse(input, parser) match {
        case Success(value, index) if input.length == index => fail(s"Expected failed parsing, but got $value")
        case Success(value, index) if input.length != index =>
        case Failure(label, index, extra) =>
      }
    }
}
