package inca.frontend.souffle

import Syntax._
import org.scalatest.funsuite.AnyFunSuite

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
    def p(src: String): Unit =
      PrettyPrinter.print(Parser.parse(src))

    p("A(a, b) :- A(a, c), A(c, b).")
    p("A(a, b) :- A(a, c), b = \"yo\".")
  }

  test("program") {
    val r = Parser.parse(
      """ .decl A(i: number, s: symbol)
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
    )

    PrettyPrinter.print(r)
  }

  test("directive") {
    val r = Parser.parse(".limitsize A(n=47)")

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

    PrettyPrinter.print(Parser.parse(Parser.rule, "A(x) :- x = 0."))
  }

  test("typeDecls") {
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

    p(".pragma \"legacy\"")
    p(".pragma \"someFlag\" \"on\"")
  }

  test("aggregator") {
    def p(src: String): Unit =
      PrettyPrinter.print(Parser.parse(Parser.aggregator, src))

    p("min x : A(x)")
    p("max    x:  A(x)   ")
    p("mean\nx\n:\nA(x)\n")
    p("sum x  :  A(x)   ")

    p("count : A(x)")
    p("count : { A(x), x > 0; x = 0 }")
    p("range(0, 5, .5)")
  }

  test("subsumptiveRule") {
    def p(src: String): Unit =
      PrettyPrinter.print(Parser.parse(Parser.subsumptiveRule, src))

    p("A(a, b) <= B(b, d) :- B(b, c), B(c, d). ")
    p("A(a, b) <= B(b, d) :- B(b, c), B(c, d). .plan 1 : (), 2 : (42), 3:(0,1,2)")
  }

  test("functorDecl") {
    def p(src: String): Unit = {
      PrettyPrinter.print(Parser.parse(Parser.functorDecl, src))
      println(Parser.parse(Parser.functorDecl, src))
    }

    p(".functor test(a:number):number ")
    p(".functor test(a:number,b:number,c:symbol):number ")
    p(".functor test(a:number):number stateful")
  }

  test("instrinsicFunc"){
    def p(src: String): Unit = {
      PrettyPrinter.print(Parser.parse(Parser.instrinsicFunc, src))
      println(Parser.parse(Parser.instrinsicFunc, src))
    }

    p("ord")
    p("to_float")
    p("to_number")
    p("to_string")
    p("to_unsigned")
    p("cat")
    p("strlen")
    p("substr")
    p("autoinc")
  }

  test("userdefinedFunc"){
    def p(src: String): Unit = {
      PrettyPrinter.print(Parser.parse(Parser.userFunc, src))
      println(Parser.parse(Parser.userFunc, src))
    }

    p("@test_func")
    p("@test_func2")
  }

  test("functors"){
    def p(src: String): Unit = {
//      PrettyPrinter.print(Parser.parse(Parser.argumentAtom, src))
      println(Parser.parse(Parser.argumentAtom, src))
    }
    println("usual functor calls")
    p("some_function(a)")
    p("this_function(123)")
    println("\n")

    println("intrinsic functor calls")
    p("ord(a)")
    p("ord(\"some text\")")
    p("to_float(\"123\")")
    p("to_float(b)")
    p("to_string(123)")
    p("to_string(some_var)")
    p("to_number(\"13\")")
    p("to_number(c)")
    p("to_unsigned(\"16\")")
    p("to_unsigned(var_name)")
    p("to_unsigned(var_name)")
    p("cat(\"left text\", \"right text\")")
    p("cat(\"left text\", cat(\"second left text\",\"right text\"))")
    p("strlen(\"left text\")")
    p("substr(\"left text\", 3, 4)")
    p("substr(cat(\"left text\", \"right text\"), 3, 4)")
    p("autoinc()")
    println("\n")

    println("user_defined functor calls")
    p("@my_function(a, b, c)")
    p("@my_function2(a)")
    p("@test_function(\"some text\")")
    p("@some_function()")
    println("\n")
  }

  test("argument") {
    def p(src: String): Unit = PrettyPrinter.print(Parser.parse(Parser.argument, src))
    def q(src: String): Unit = println(Parser.parse(Parser.argument, src))

    p("nil")
    q("xyz")
    q("_")
    p("bnot x")
    p("lnot y")
    q("- z")
    q("-z")
    p("foo(.5)")
    p("$foo")
    p("$foo(\"hi\")")
    p("$foo (0)")
    p("\"whats up\"")
    p("max q : { q < 0 }")
    p("range(0, 1)")
    p("42")
    p(" [ a  ,  b , 42 , count : A(x)  ] ")
    p("as  ( x ,  number )")
    q("as(x, number)")
    q(" ( _ ) ")

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
