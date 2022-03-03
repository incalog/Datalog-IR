package inca.frontend.souffle

import Syntax._
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {
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
}
