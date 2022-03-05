package inca.frontend.souffle

import Syntax._
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {
  def assertParseFail(f: => Any): Unit = {
    try f
    catch {
      case ParseException(_) => assert(0 == 0)
      case _: Throwable => assert(0 == 1)
    }
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
        | .decl Foo2(x: symbol, y: number) inline no_inline
        | .decl Foo3(x: symbol, y: number) magic choice-domain x, (y, z)
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
      Seq(Attribute("x", SymbolType), Attribute("y", NumberType)),
      Seq(InlineQualifier, NoInlineQualifier)
    ))

    assert(decls(2) == RelationDecl(
      "Foo3",
      Seq(Attribute("x", SymbolType), Attribute("y", NumberType)),
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
  }

  test("rules") {
    val r = Parser.parse(
      """ A(a, b) :- A(a, c), A(c, b).
        |""".stripMargin
    )

    println(PrettyPrinter.stringify(r))
  }

  test("program") {
    val r = Parser.parse(
      """ .decl A(i: number, s: symbol)
        |
        | A(0, "leon").
        | A(1, "sina").
        |
        | A(i, s) :- A(0, s).
        |""".stripMargin
    )

    PrettyPrinter.print(r)
  }

  test("directive") {
    val r = Parser.parse(
      """ .limitsize A(n=47)
        |""".stripMargin
    )

    println(PrettyPrinter.stringify(r))
  }

  test("constraints") {
    PrettyPrinter.print(Parser.parse(Parser.constraint, "x = 0"))
    PrettyPrinter.print(Parser.parse(Parser.constraint, "x != 0"))
    PrettyPrinter.print(Parser.parse(Parser.constraint, "x <= 0"))
    PrettyPrinter.print(Parser.parse(Parser.constraint, "x < 0"))

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
}
