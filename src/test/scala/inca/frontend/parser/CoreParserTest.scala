package inca.frontend.parser

import inca.frontend.core.Core
import inca.frontend.core.Core._
import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

/**
  * Test class for the IncA core language parser @see CoreParser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class CoreParserTest extends AnyFunSuite {

  test("test TypeAnno") {
    def test_run(t: TypeAnno) = {
      test_helper(CoreParser().typeAnno(_))(t.prettyprint, t)
      test_helper_negative(CoreParser().typeAnno(_))(s"   ${t.prettyprint}")
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
    ).map(test_run(_))
  }

  test("test Visibility") {
    def positive(v: Visibility)(t: String) = test_helper(CoreParser().visibility(_))(t, v)
    def negative(v: String) = test_helper_negative(CoreParser().visibility(_))(v)

    Seq("public", "   public", " public").map(positive(Public)(_))
    Seq("private", "   private", " private").map(positive(Private)(_))
    Seq("provate", "  prbplic", "   plsplapic", "bluplic").map(negative)
  }

  test("test TIterable") {
    def test_run(inp : String, cmp : TIterable) = {
      test_helper(CoreParser().tIterable(_))(inp, cmp)
      test_helper_negative(CoreParser().tIterable(_))(s"Q$inp")
    }

    test_run("List[node]", TList(TAnyLinked))
    test_run("List[apf3l]", TList(TNode("apf3l")))
    test_run("Enum[node]", TEnumeration(TAnyLinked))
    test_run("Enum[br0t]", TEnumeration(TNode("br0t")))
  }

  test("test IntLiteral") {
    def test_run(input: String, cmp: Int) =
      test_helper(CoreParser().intLiteral(_))(input, IntLiteral(cmp))

    test_run("0", 0)
    test_run("1", 1)
    test_run("12", 12)
    test_run("-1", -1)
    test_run("-12", -12)
  }

  test("test BooleanLiteral") {
    def test_run(b: Boolean): Unit =
      test_helper(CoreParser().booleanLiteral(_))(b.toString, BooleanLiteral(b))
    test_run(true)
    test_run(false)
  }

  test("test LongLiteral") {
    def test_run(input: String, r: Long) =
      test_helper(CoreParser().longLiteral(_))(input, LongLiteral(r))
    test_run("1L", 1L)
    test_run("-1L", -1L)
    test_run("11L", 11L)
    test_run("-11L", -11L)
  }

  test("test DoubleLiteral") {
    def test_run(input: String, r: Double) =
      test_helper(CoreParser().doubleLiteral(_))(input, DoubleLiteral(r))
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
    def test_run(t: TypeAnno) =
      test_helper(CoreParser().annoParam(_))(
        s"(param:${t.prettyprint})",
        AnnoParam(Some("param"), t)
      )

    Seq(TBool, TDouble, TString, TInt, TLong, TAnyLinked, TNode("br0t")).map(test_run)
  }

  test("test AnnoParam without name") {
    def test_run(t: TypeAnno) =
      test_helper(CoreParser().annoParam(_))(s"${t.prettyprint}", AnnoParam(None, t))

    Seq(TBool, TDouble, TString, TInt, TLong, TAnyLinked, TNode("br0t")).map(test_run)
  }

  test("test Link core-links") {
    def test_run = test_helper(CoreParser().link(TNode("node"))(_))

    test_run("parent", ParentLink)
    test_run("children", ChildrenLink)
    test_run("next", NextLink)
    test_run("prev", PreviousLink)
    test_run("name", NamedLink(TNode("node"), "name"))
  }

  test("test Var") {
    def test_run(input: String) = test_helper(CoreParser().varExp(_))(input, Var(input))

    Seq("variable", "br0t").map(test_run(_))
  }

  test("test Constant") {

    def test_run(inp: String, cmp: Literal) =
      test_helper(CoreParser().constantExp(_))(inp, Constant(cmp))

    test_run("1", IntLiteral(1))
    test_run("true", BooleanLiteral(true))
    test_run("1L", LongLiteral(1))
    test_run("unit", UnitLiteral)
    test_run("42d", DoubleLiteral(42d))
    test_run("\"hello world\"", StringLiteral("hello world"))
  }

  test("test Exp single") {
    def test_run(tree: Exp) =
      test_helper(CoreParser().exp(_))(tree.prettyprint("  "), tree)

    test_run(Eq(Var("x"), Var("y")))
    test_run(Neq(Var("x"), Var("y")))
    test_run(Def(Var("x")))
    test_run(Undef(Var("x")))
    test_run(InstanceOf(Var("x"), TBool))
    test_run(NotInstanceOf(Var("x"), TBool))
    test_run(PathAccess(Var("xyz"), ParentLink))
    test_run(PathAccess(Var("test"), NamedLink(TNode("dummy"), "property")))
    test_run(
      Aggregate(
        DataOp(Some("br0t"), "with"),
        DataOp(Some("cheese"), "and"),
        None,
        Call("butter", Seq.empty, false)
      )
    )
  }

  test("test identifier") {
    def positive(v: String) =
      parse(v, CoreParser().identifier(_)) match {
        case Failure(label, index, extra) => fail()
        case Success(value, index)        => assert(value === v)
      }
    def negative(v: String) =
      parse(v, CoreParser().identifier(_)) match {
        case Failure(label, index, extra) => {}
        case Success(value, index)        => fail()
      }

    Seq("kuch3n", "k3k53", "t33", "kl33", "br0t", "s0nn3nblum3").map(positive)
    Seq("3553n", "71nux", " ", "53h3n").map(negative)
  }

  test("test Eval params") {
    def test_run(code: String, vars: Seq[Name]) = {
      val parsed = parse(s"eval($code)", CoreParser().evalExp(_))
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
    test_run(code, Seq("x", "y", "z", "inf", "code", "ten"))

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
    test_run(code1, Seq("some", "pen", "obj"))
  }

  test("test Exp combined") {
    // @todo Add more test cases.
    def test_run = test_helper[CoreExp](CoreParser().exp(_))

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
    test_run(
      "x.parent",
      PathAccess(Var("x"), ParentLink)
    )
    test_run(
      "x.children",
      PathAccess(Var("x"), ChildrenLink)
    )
    test_run(
      "x.next",
      PathAccess(Var("x"), NextLink)
    )
    test_run(
      "x.prev",
      PathAccess(Var("x"), PreviousLink)
    )
    test_run(
      "x.size",
      PathAccess(Var("x"), SizeLink)
    )
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
    def test_run(input: String, cmp: TTuple) = test_helper[TTuple](CoreParser().tTuple(_))

    test_run("(int,string)", TTuple(Seq(TInt, TString)))
    test_run("(int, string)", TTuple(Seq(TInt, TString)))
    test_run("(int string)", TTuple(Seq(TInt, TString)))
    test_run(" ( int , string ) ", TTuple(Seq(TInt, TString)))
    test_run("(double)", TTuple(Seq(TDouble)))
    test_run("Unit", TTuple(Seq.empty))
  }

  test("test Statement") {
    def test_run(tree: Statement) =
      test_helper(CoreParser().statement(_))(tree.prettyprint("  "), tree)

    test_run(Yield(Var("x")))
    test_run(Core.Fail)
    test_run(Values("x", TBool))
    test_run(Assign(Seq("x"), Var("y")))
    test_run(Assign(Seq("x", "y", "abc"), Var("zs")))
  }

  test("test Body") {
    def test_run(input: String, cmp: Body) = {
      test_helper[Body](CoreParser().body(_))(input, cmp)
      test_helper[Body](CoreParser().body(_))(input.replaceAll("\n", "\r\n"), cmp)
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
    // @todo Add more test cases.
    def test_run(input: String, cmp: PatternFunction) = {
      test_helper[PatternFunction](CoreParser().patternFunction(_))(input, cmp)
      test_helper[PatternFunction](CoreParser().patternFunction(_))(
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
    def test_run = test_helper[Module](CoreParser().module(_))

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

  test("test DataOp") {
    // @todo Add more tests.
    def test_run = test_helper(CoreParser().dataOp(_))

    test_run("br0t.br0t", DataOp(Some("br0t"), "br0t"))
    test_run("br0t", DataOp(None, "br0t"))
  }

  test("test Eval") {
    def test_run(input: String, vars: Set[String], cmp_code : String) =
      parse(input, CoreParser().evalExp(_)) match {
        case Success(Eval(ss, rt, code), index) => {
          assert(cmp_code === code)
          assert((Set.empty[String] ++ ss) === vars)
          assert(rt === TNode("dummy"))
        }
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }

    test_run("eval(x + 2)", Set("x"), "x + 2")
    test_run("eval(x + y + p)", Set("x", "y", "p"), "x + y + p")
    // test_run(
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

  private def test_helper[T](parser: P[_] => P[Any]) =
    (input: String, cmp: T) => {
      parse(input, parser) match {
        case Success(value, index)        => assert(value === cmp)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }

  private[parser] def test_helper_negative[T](parser: P[_] => P[Any]) =
    (input: String) => {
      parse(input, parser) match {
        case Success(value, index)        => fail(s"$value, $index")
        case Failure(label, index, extra) =>
      }
    }
}
