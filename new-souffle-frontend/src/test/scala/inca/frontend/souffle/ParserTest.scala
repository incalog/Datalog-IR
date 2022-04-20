package inca.frontend.souffle

import Syntax._
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class ParserTest extends AnyFunSuite {
  def assertParseFail(f: => Any): Unit = {
    try f
    catch {
      case e: ParseException => println("Correct parse failure: " + e.message)
      case _: Throwable => assert(0 == 1)
    }
  }

  def assertEqual[A](head: A, as: A*): Unit = {
    as.foreach(a => assert(a == head))
    PrettyPrinter.print(head)
  }

  def removeWhitespace(s: String): String = {
    //s.replaceAll("\\n\\t ", "")
    //s.filter(_ > ' ')
    s.split('\n').map(_.trim.filter(_ > ' ')).mkString
  }

  test("constants") {
    def p(src: String): Constant = Parser.parse(Parser.constant, src)

    print(Seq(
      p("5.42"),
      p("+5.42"),
      p("-5.42"),
      p("5.42"),
      p(".42"),
      p("6424"),
      p("3526"),
      p("-1363"),
      p("\"waddup\""),
    ))
  }

  test("relation declaration") {
    val r = Parser.parse(
      """
        | .decl Foo1(x: hi)
        | .decl Foo2(x: symbol, ?y: number) inline no_inline overridable
        | .decl Foo3(x: symbol, ?y: number) magic choice-domain x, (y, z)
        | .decl Nullary()
        |""".stripMargin
    )

    val decls = r.map(_.asInstanceOf[RelationDecl])

    assert(decls.head == RelationDecl(
      "Foo1",
      Seq(Attribute("x", DeclaredType("hi")))
    ))

    assert(decls(1) == RelationDecl(
      "Foo2",
      Seq(Attribute("x", SymbolType), Attribute("?y", NumberType)),
      Seq(InlineQualifier, NoInlineQualifier, OverridableQualifier)
    ))

    assert(decls(2) == RelationDecl(
      "Foo3",
      Seq(Attribute("x", SymbolType), Attribute("?y", NumberType)),
      Seq(MagicQualifier),
      Some(ChoiceDomain(Seq("x", "y", "z")))
    ))

    assert(decls.last == RelationDecl("Nullary", Seq()))
    assert(decls.last.isNullary)

    println(PrettyPrinter.stringify(r))
  }

  test("literals") {
    def f(src: String): Float = Parser.parse(Parser.Literals.float, src)
    assert(f("42.8") == 42.8f)
    assert(f("+42.8") == 42.8f)
    assert(f("-42.8") == -42.8f)
    assert(f(".8") == 0.8f)
    assert(f("-.8") == -0.8f)
    assert(f("42e-3") == 0.042f)

    assert(Parser.parse(Parser.Literals.string, "\"Hallo!\"") == "Hallo!")
  }

  test("facts") {
    val r = Parser.parse(
      """ A(0, 1).
        | A().
        | B("hallo", 42).
        |""".stripMargin
    )

    println(PrettyPrinter.stringify(r))

    assert(r == Seq(
      Fact(Atom("A", Seq(ArgumentConstant(ConstantUnsigned(0)), ArgumentConstant(ConstantUnsigned(1))))),
      Fact(Atom("A", Seq())),
      Fact(Atom("B", Seq(ArgumentConstant(ConstantString("hallo")), ArgumentConstant(ConstantUnsigned(42))))),
    ))
  }

  test("rules") {

    val input1 = "A(a, b) :- A(a, c), A(c, b)."
    val input1p = PrettyPrinter.stringify(Parser.parse(input1))
    assert(input1 == input1p)

    val input2 = "A(a, b) :- A(a, c), b = \"yo\"."
    val input2p = PrettyPrinter.stringify(Parser.parse(input2))
    assert(input2 == input2p)

  }

  test("program") {

    val ipt = """ .decl A(i: number, s: symbol)
                |
                | A(0, "leon").
                | A(1, "sina").
                |
                | A(i, s) :- A(0, s).
                |
                | .comp Tree<T> : Plant {
                |   .decl Leaf(n: T)
                |   Leaf(0).
                |   Leaf(1).
                |   Leaf(2).
                | }
                |
                | .init myTree = Tree<number>
                |
                | .pragma "legacy"
                | .pragma "someFlag" "on"
                |
                | B(x) :- A(  count : { Prime(x) }  ).
                | B(x):-A(count:{Prime(x)}).
                |
                | A(x) <= B(x) :- C(x).
                | A(x) <= B(x) :- C(x). .plan 1:(42)
                |""".stripMargin

    val iptp = PrettyPrinter.stringify(Parser.parse(ipt))

    assert(removeWhitespace(ipt) == removeWhitespace(iptp))
  }

  test("directive") {
    val r = Parser.parse(".limitsize A(n=47)")

    assert(r == Seq(Directive(qualifier = DirectiveQualifierLimitsize,
      qualifiedNames = Seq("A"),
      params = Map("n" -> DirectiveValueNumber(47)))))

    //assert(PrettyPrinter.stringify(r) == ".limitsize A(n=47)")
    println(PrettyPrinter.stringify(r))
  }

  test("constraints") {
    def p(src: String): Unit =
      PrettyPrinter.print(Parser.parse(Parser.constraint, src))

    p("x = 0")
    p("x != 0")
    p("x <= 0")
    p("x < 0")

    p("match(x, y)")
    p("contains(\"a\", \"bab\")")
    assertParseFail(p("foo(x, y)"))

    val ipt = "A(x) :- x = 0."
    val iptp = PrettyPrinter.stringify(Parser.parse(ipt))

    assert(ipt == iptp)

    PrettyPrinter.print(Parser.parse(Parser.rule, "A(x) :- x = 0."))
  }

  test("typeDecls") {

    val ipt1 = ".type A <: B"
    val ipt1p = PrettyPrinter.stringify(Parser.parse(ipt1))
    assert(ipt1 == ipt1p)

    val ipt2 = ".type A = B | C | number"
    val ipt2p = PrettyPrinter.stringify(Parser.parse(ipt2))
    assert(ipt2 == ipt2p)

    val ipt3 = ".type A = [ a: float, b: symbol ]"
    val ipt3p = PrettyPrinter.stringify(Parser.parse(ipt3))
    assert(ipt3 == ipt3p)

    val ipt4 = ".type A = B { b: number } | C { c: symbol }"
    val ipt4p = PrettyPrinter.stringify(Parser.parse(ipt4))
    assert(ipt4 == ipt4p)

    PrettyPrinter.print(Parser.parse(Parser.typeDecl, ".type A <: B"))
    PrettyPrinter.print(Parser.parse(Parser.typeDecl, ".type A = B | C | number"))
    PrettyPrinter.print(Parser.parse(Parser.typeDecl, ".type A = [ a: float, b: symbol ]"))
    PrettyPrinter.print(Parser.parse(Parser.typeDecl, ".type A = B { b: number } | C { c: symbol }"))
  }

  test("componentDecl") {
    def p(src: String): Unit =
      PrettyPrinter.print(Parser.parse(Parser.componentDecl, src))

    p(""" .comp Tree : Plant<Wood> {
        |   .decl Leaf(n: symbol)
        |   Leaf("a").
        |   Leaf("b").
        | }
        |""".stripMargin)

    p(""" .comp Soup {
        |   .type A = B
        | }
        |""".stripMargin)
  }

  test("componentInit") {
    def p(src: String): Unit =
      PrettyPrinter.print(Parser.parse(Parser.componentInit, src))

    p(".init Tree = Bonsai<A>")
    assertParseFail(p(".init Tree = Bonsai <A>"))
  }

  test("pragma") {
    def p(src: String): Unit =
      PrettyPrinter.print(Parser.parse(Parser.pragma, src))

    val ipt1 = ".pragma \"legacy\""
    val ipt1p = PrettyPrinter.stringify(Parser.parse(ipt1))
    assert(ipt1 == ipt1p)

    val ipt2 = ".pragma \"someFlag\" \"on\""
    val ipt2p = PrettyPrinter.stringify(Parser.parse(ipt2))
    assert(ipt2 == ipt2p)
    p(ipt1)
    p(ipt2)
  }

  test("aggregator") {
    def p(src: String): String = {
      PrettyPrinter.print(Parser.parse(Parser.aggregator, src))
      println(Parser.parse(Parser.aggregator, src))
      PrettyPrinter.stringify(Parser.parse(Parser.aggregator, src))
    }

    val ipt1 = "min x : A(x)"
    val ipt1p = p(ipt1)
    assert(removeWhitespace(ipt1) == removeWhitespace(ipt1p))

    val ipt2 = "min x+y : { A(x), A(y), C(y) }"
    val ipt2p = p(ipt2)
    assert(removeWhitespace(ipt2) == removeWhitespace(ipt2p))

    val ipt3 = "max    x:  A(x)   "
    val ipt3p = p(ipt3)
    assert(removeWhitespace(ipt3) == removeWhitespace(ipt3p))

    val ipt4 = "mean\nx\n:\nA(x)\n"
    val ipt4p = p(ipt4)
    assert(removeWhitespace(ipt4) == removeWhitespace(ipt4p))

    val ipt5 = "sum x  :  A(x)   "
    val ipt5p = p(ipt5)
    assert(removeWhitespace(ipt5) == removeWhitespace(ipt5p))

    val ipt6 = "count : A(x)"
    val ipt6p = p(ipt6)
    assert(removeWhitespace(ipt6) == removeWhitespace(ipt6p))

    val ipt7 = "count : { A(x), x > 0; x = 0 }"
    val ipt7p = p(ipt7)
    assert(removeWhitespace(ipt7) == removeWhitespace(ipt7p))

    val ipt8 = "range(0, 5, .5)"
    val ipt8p = Parser.parse(Parser.aggregator, ipt8)
    assert(ipt8p == AggregatorRange(ArgumentConstant(ConstantUnsigned(0)),
      ArgumentConstant(ConstantUnsigned(5)),Some(ArgumentConstant(ConstantFloat(0.5f)))))
  }

  test("subsumptiveRule") {
    def p(src: String): String = {
      PrettyPrinter.print(Parser.parse(Parser.subsumptiveRule, src))
      PrettyPrinter.stringify(Parser.parse(Parser.subsumptiveRule, src))
    }

    val ipt1 = "A(a, b) <= B(b, d) :- B(b, c), B(c, d). "
    val ipt1p = p(ipt1)
    assert(removeWhitespace(ipt1) == removeWhitespace(ipt1p))

    val ipt2 = "A(a, b) <= B(b, d) :- B(b, c), B(c, d). .plan 1 : (), 2 : (42), 3:(0,1,2)"
    val ipt2p = p(ipt2)
    assert(removeWhitespace(ipt2) == removeWhitespace(ipt2p))

  }

  test("functorDecl") {
    def p(src: String): String = {
      PrettyPrinter.print(Parser.parse(Parser.functorDecl, src))
      println(Parser.parse(Parser.functorDecl, src))
      PrettyPrinter.stringify(Parser.parse(Parser.functorDecl, src))
    }

    val ipt1 = ".functor test(a:number):number "
    val ipt1p = p(ipt1)
    assert(removeWhitespace(ipt1) == removeWhitespace(ipt1p))

    val ipt2 = ".functor test(a:number,b:number,c:symbol):number "
    val ipt2p = p(ipt2)
    assert(removeWhitespace(ipt2) == removeWhitespace(ipt2p))

    val ipt3 = ".functor test(a:number):number stateful"
    val ipt3p = p(ipt3)
    assert(removeWhitespace(ipt3) == removeWhitespace(ipt3p))
  }

  test("instrinsicFunc"){
    def p(src: String): String = {
      PrettyPrinter.print(Parser.parse(Parser.instrinsicFunc, src))
      println(Parser.parse(Parser.instrinsicFunc, src))
      PrettyPrinter.stringify(Parser.parse(Parser.instrinsicFunc, src))
    }

    val ipOrd = "ord"
    val ipOrdp = p(ipOrd)
    assert(ipOrd == ipOrdp)

    val ipFlt = "to_float"
    val ipFltp = p(ipFlt)
    assert(ipFlt == ipFltp)

    val ipNum = "to_number"
    val ipNump = p(ipNum)
    assert(ipNum == ipNump)

    val ipStr = "to_string"
    val ipStrp = p(ipStr)
    assert(ipStr == ipStrp)

    val ipUns = "to_unsigned"
    val ipUnsp = p(ipUns)
    assert(ipUns == ipUnsp)

    val ipCat = "cat"
    val ipCatp = p(ipCat)
    assert(ipCat == ipCatp)

    val ipStl = "strlen"
    val ipStlp = p(ipStl)
    assert(ipStl == ipStlp)

    val ipSub = "substr"
    val ipSubp = p(ipSub)
    assert(ipSub == ipSubp)

    val ipAui = "autoinc"
    val ipAuip = p(ipAui)
    assert(ipAui == ipAuip)

  }

  test("userdefinedFunc"){
    def p(src: String): String = {
      PrettyPrinter.print(Parser.parse(Parser.userFunc, src))
      println(Parser.parse(Parser.userFunc, src))
      PrettyPrinter.stringify(Parser.parse(Parser.userFunc, src))
    }

    val ipt1 = "@test_func"
    val ipt1p = p(ipt1)
    assert(ipt1 == ipt1p)

    val ipt2 = "@test_func2"
    val ipt2p = p(ipt2)
    assert(ipt2 == ipt2p)

  }

  test("functors"){
    def p(src: String): String = {
//      PrettyPrinter.print(Parser.parse(Parser.argumentAtom, src))
      //println(Parser.parse(Parser.argumentAtom, src))
      PrettyPrinter.stringify(Parser.parse(Parser.argumentAtom, src))
    }


    println("usual functor calls")

    val iptF1 = "@some_function(a)"
    val iptF1p = Parser.parse(Parser.argumentAtom, iptF1)
    assert(iptF1p == ArgumentUserDefinedFunc(UserDefinedFunctor("some_function"),
      List(ArgumentVariable("a"))))

    val iptF2 = "@this_function(123)"
    val iptF2p = Parser.parse(Parser.argumentAtom, iptF2)
    assert(iptF2p == ArgumentUserDefinedFunc(UserDefinedFunctor("this_function"),
      List(ArgumentConstant(ConstantUnsigned(123)))))

    println("\n")

    println("intrinsic functor calls")

    val iptI1 = "ord(a)"
    val iptI1p = p(iptI1)
    assert(removeWhitespace(iptI1) == removeWhitespace(iptI1p))

    val iptI2 = "ord(\"some text\")"
    val iptI2p = p(iptI2)
    assert(removeWhitespace(iptI2) == removeWhitespace(iptI2p))

    val iptI3 = "to_float(\"123\")"
    val iptI3p = p(iptI3)
    assert(removeWhitespace(iptI3) == removeWhitespace(iptI3p))

    val iptI4 = "to_float(b)"
    val iptI4p = p(iptI4)
    assert(removeWhitespace(iptI4) == removeWhitespace(iptI4p))

    val iptI5 = "to_string(123)"
    val iptI5p = p(iptI5)
    assert(removeWhitespace(iptI5) == removeWhitespace(iptI5p))

    val iptI6 = "to_string(some_var)"
    val iptI6p = p(iptI6)
    assert(removeWhitespace(iptI6) == removeWhitespace(iptI6p))

    val iptI7 = "to_number(\"13\")"
    val iptI7p = p(iptI7)
    assert(removeWhitespace(iptI7) == removeWhitespace(iptI7p))

    val iptI8 = "to_number(c)"
    val iptI8p = p(iptI8)
    assert(removeWhitespace(iptI8) == removeWhitespace(iptI8p))

    val iptI9 = "to_unsigned(\"16\")"
    val iptI9p = p(iptI9)
    assert(removeWhitespace(iptI9) == removeWhitespace(iptI9p))

    val iptI10 = "to_unsigned(var_name)"
    val iptI10p = p(iptI10)
    assert(removeWhitespace(iptI10) == removeWhitespace(iptI10p))

    val iptI11 = "cat(\"left text\", \"right text\")"
    val iptI11p = p(iptI11)
    assert(removeWhitespace(iptI11) == removeWhitespace(iptI11p))

    val iptI12 = "cat(\"left text\", cat(\"second left text\",\"right text\"))"
    val iptI12p = p(iptI12)
    assert(removeWhitespace(iptI12) == removeWhitespace(iptI12p))

    val iptI13 = "strlen(\"left text\")"
    val iptI13p = p(iptI13)
    assert(removeWhitespace(iptI13) == removeWhitespace(iptI13p))

    val iptI14 = "substr(\"left text\", 3, 4)"
    val iptI14p = p(iptI14)
    assert(removeWhitespace(iptI14) == removeWhitespace(iptI14p))

    val iptI15 = "substr(cat(\"left text\", \"right text\"), 3, 4)"
    val iptI15p = p(iptI15)
    assert(removeWhitespace(iptI15) == removeWhitespace(iptI15p))

    val iptI16 = "substr(cat(\"left text\", \"right text\"), 3, 4)"
    val iptI16p = p(iptI16)
    assert(removeWhitespace(iptI16) == removeWhitespace(iptI16p))

    println("\n")

    println("user_defined functor calls")

    val iptU1 = "@my_function(a, b, c)"
    val iptU1p = Parser.parse(Parser.argumentAtom, iptU1)
    assert(iptU1p == ArgumentUserDefinedFunc(UserDefinedFunctor("my_function"),
      List(ArgumentVariable("a"), ArgumentVariable("b"), ArgumentVariable("c"))))

    val iptU2 = "@my_function2(a)"
    val iptU2p = Parser.parse(Parser.argumentAtom, iptU2)
    assert(iptU2p == ArgumentUserDefinedFunc(UserDefinedFunctor("my_function2"),
      List(ArgumentVariable("a"))))

    val iptU3 = "@test_function(\"some text\")"
    val iptU3p = Parser.parse(Parser.argumentAtom, iptU3)
    assert(iptU3p == ArgumentUserDefinedFunc(UserDefinedFunctor("test_function"),
      List(ArgumentConstant(ConstantString("some text")))))

    val iptU4 = "@some_function()"
    val iptU4p = Parser.parse(Parser.argumentAtom, iptU4)
    assert(iptU4p == ArgumentUserDefinedFunc(UserDefinedFunctor("some_function"),List()))

  }

  test("argument") {
    def p(src: String): String = {
      PrettyPrinter.print(Parser.parse(Parser.argument, src))
      println(Parser.parse(Parser.argument, src))
      PrettyPrinter.stringify(Parser.parse(Parser.argument, src))
    }

    val ipt1 = "nil"
    val ipt1p = p(ipt1)
    assert(removeWhitespace(ipt1) == removeWhitespace(ipt1p))

    val ipt2 = "xyz"
    val ipt2p = p(ipt2)
    assert(removeWhitespace(ipt2) == removeWhitespace(ipt2p))

    val ipt3 = "_"
    val ipt3p = p(ipt3)
    assert(removeWhitespace(ipt3) == removeWhitespace(ipt3p))

    val ipt4 = "bnot x"
    val ipt4p = p(ipt4)
    assert(removeWhitespace(ipt4) == removeWhitespace(ipt4p))

    val ipt5 = "lnot y"
    val ipt5p = p(ipt5)
    assert(removeWhitespace(ipt5) == removeWhitespace(ipt5p))

    val ipt6 = "- z"
    val ipt6p = p(ipt6)
    assert(removeWhitespace(ipt6) == removeWhitespace(ipt6p))

    val ipt7 = "-z"
    val ipt7p = p(ipt7)
    assert(removeWhitespace(ipt7) == removeWhitespace(ipt7p))

    val ipt8 = "@foo(.5)"
    val ipt8p = Parser.parse(Parser.argument, ipt8)
    assert(ipt8p == ArgumentUserDefinedFunc(UserDefinedFunctor("foo"),
      List(ArgumentConstant(ConstantFloat(0.5f)))))

    val ipt9 = "$foo"
    val ipt9p = p(ipt9)
    assert(removeWhitespace(ipt9) == removeWhitespace(ipt9p))

    val ipt10 = "$foo(\"hi\")"
    val ipt10p = p(ipt10)
    assert(removeWhitespace(ipt10) == removeWhitespace(ipt10p))

    val ipt11 = "$foo (0)"
    val ipt11p = p(ipt11)
    assert(removeWhitespace(ipt11) == removeWhitespace(ipt11p))

    val ipt12 = "\"whats up\""
    val ipt12p = p(ipt12)
    assert(removeWhitespace(ipt12) == removeWhitespace(ipt12p))

    val ipt13 = "max q : { q < 0 }"
    val ipt13p = p(ipt13)
    assert(removeWhitespace(ipt13) == removeWhitespace(ipt13p))

    val ipt14 = "range(0, 1)"
    val ipt14p = Parser.parse(Parser.argument, ipt14)
    assert(ipt14p == ArgumentAggregator(AggregatorRange(ArgumentConstant
    (ConstantUnsigned(0)),ArgumentConstant(ConstantUnsigned(1)),None)))

    val ipt15 = "42"
    val ipt15p = p(ipt15)
    assert(removeWhitespace(ipt15) == removeWhitespace(ipt15p))

    val ipt16 = " [ a  ,  b , 42 , count : A(x)  ] "
    val ipt16p = p(ipt16)
    assert(removeWhitespace(ipt16) == removeWhitespace(ipt16p))

    val ipt17 = "as  ( x ,  number )"
    val ipt17p = p(ipt17)
    assert(removeWhitespace(ipt17) == removeWhitespace(ipt17p))

    val ipt18 = "as(x, number)"
    val ipt18p = p(ipt18)
    assert(removeWhitespace(ipt18) == removeWhitespace(ipt18p))

    val ipt19 = " ( _ ) "
    val ipt19p = p(ipt19)
    assert(removeWhitespace(ipt19) == removeWhitespace(ipt19p))

    assertEqual(
      ArgumentAlias(ArgumentVariable("x"), NumberType),
      Parser.parse(Parser.argument, "as(x, number)"),
      Parser.parse(Parser.argument, "as ( x , number )"),
    )
  }

  test("large program") {
    import scala.io.Source

    val location = "new-souffle-frontend/src/test/scala/inca/frontend/souffle/context-insensitive.dl"
    val buffer = Source.fromFile(location)
    val src = buffer.getLines().mkString("\n")
    buffer.close()

    val program = Parser.parse(src)
    PrettyPrinter.print(program)
  }
}
