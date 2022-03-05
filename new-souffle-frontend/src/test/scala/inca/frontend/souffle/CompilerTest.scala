package inca.frontend.souffle

import Syntax._
import inca.backend.ir.Datalog
import inca.util.Scala
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.{Term, XtensionQuasiquoteTerm}

class CompilerTest extends AnyFunSuite {

  def assertFail(f: => Any): Unit = {
    try f
    catch {
      case _: AssertionError =>
      case _: Throwable => assert(0 == 1)
    }
  }

  test("relationDecl") {
    val c = new Compiler()
    val decl = RelationDecl("A", Seq(Attribute("x", NumberType)))

    c.compileRelationDecl(decl)

    PrettyPrinter.print(c.relationDecls.values)
    PrettyPrinter.print(c.patterns.values)
  }

  test("fact") {
    val c = new Compiler()

    val decl = RelationDecl("A", Seq(Attribute("x", FloatType)))

    c.relationDecls += QualifiedName("A") -> decl
    c.patterns +=
      QualifiedName("A") ->
        Datalog.Pattern(None, "A", decl.attributes.map(c.compileAttribute), Seq())

    c.compileFact(Parser.parse(Parser.fact, "A(5.0)."))

    PrettyPrinter.print(c.relationDecls.values)
    PrettyPrinter.print(c.patterns.values)
  }

  test("factFail") {
    val c = new Compiler

    assertFail(c.compileFact(Parser.parse(Parser.fact, "A(x, 0).")))

    c.compileRelationDecl(Parser.parse(Parser.relationDecl, ".decl A(x: number)"))
    assertFail(c.compileFact(Parser.parse(Parser.fact, "A(1, 2).")))
  }

  test("subtyping") {
    val c = new Compiler

    assert(c.isSubtype(UnsignedType, NumberType))
    assert(c.isSubtype(NumberType, FloatType))
    assert(c.isSubtype(UnsignedType, FloatType))
  }

  test("rule") {
    val c = new Compiler

    c.compileRelationDecl(Parser.parse(Parser.relationDecl, ".decl A(x: number, y: number)"))

    c.compileRule(Parser.parse(Parser.rule, "A(x, y) :- x = 0, y = 1."))
    c.compileRule(Parser.parse(Parser.rule, "A(x, y) :- x = 5, (y = 1; y = 7)."))
    c.compileRule(Parser.parse(Parser.rule, "A(5, y) :- y = 1."))

    PrettyPrinter.print(c.patterns.values)
  }
}
