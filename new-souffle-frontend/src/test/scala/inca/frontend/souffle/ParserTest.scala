package inca.frontend.souffle

import inca.frontend.souffle.Syntax.{ChoiceDomain, DeclaredType, InlineQualifier, MagicQualifier, NoInlineQualifier, NumberType, Relation, RelationAttribute, SymbolType}
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

    val decls = r.map(_.asInstanceOf[Relation])

    assert(decls.head == Relation(
      "Foo1",
      Seq(RelationAttribute("x", DeclaredType("hi")))
    ))

    assert(decls(1) == Relation(
      "Foo2",
      Seq(RelationAttribute("x", SymbolType), RelationAttribute("y", NumberType)),
      Seq(InlineQualifier, NoInlineQualifier)
    ))

    assert(decls(2) == Relation(
      "Foo3",
      Seq(RelationAttribute("x", SymbolType), RelationAttribute("y", NumberType)),
      Seq(MagicQualifier),
      Some(ChoiceDomain(Seq("x", "y", "z")))
    ))

    assert(decls.last == Relation("Nullary", Seq()))
    assert(decls.last.isNullary)

    println(r.mkString("\n"))
  }
}
