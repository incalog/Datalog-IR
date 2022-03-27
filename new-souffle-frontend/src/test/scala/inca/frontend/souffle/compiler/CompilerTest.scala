package inca.frontend.souffle.compiler

import inca.backend.ir.Datalog
import inca.frontend.souffle.Syntax._
import inca.frontend.souffle.{Parser, PrettyPrinter, compiler}
import org.scalatest.funsuite.AnyFunSuite

class CompilerTest extends AnyFunSuite {

  def assertFail(f: => Any): Unit = {
    try f
    catch {
      case _: AssertionError =>
      case _: Throwable => assert(0 == 1)
    }
  }

  test("relationDecl") {
    val c = new compiler.Compiler()
    val decl = RelationDecl("A", Seq(Attribute("x", NumberType)))

    c.compileRelationDecl(decl)

    PrettyPrinter.print(c.relationDecls.values)
    PrettyPrinter.print(c.patterns.values)
  }

  test("fact") {
    val c = new compiler.Compiler()

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
    val c = new compiler.Compiler

    assertFail(c.compileFact(Parser.parse(Parser.fact, "A(x, 0).")))

    c.compileRelationDecl(Parser.parse(Parser.relationDecl, ".decl A(x: number)"))
    assertFail(c.compileFact(Parser.parse(Parser.fact, "A(1, 2).")))
  }

  test("subtyping") {
    val c = new compiler.Compiler

    assert(c.isSubtype(UnsignedType, NumberType))
    assert(c.isSubtype(NumberType, FloatType))
    assert(c.isSubtype(UnsignedType, FloatType))
  }

  test("rule") {
    val c = new compiler.Compiler

    c.compileRelationDecl(Parser.parse(Parser.relationDecl, ".decl A(x: number, y: number)"))

    c.compileRule(Parser.parse(Parser.rule, "A(x, y) :- x = 0, y = 1."))
    c.compileRule(Parser.parse(Parser.rule, "A(x, y) :- x = 5, (y = 1; y = 7)."))
    c.compileRule(Parser.parse(Parser.rule, "A(5, y) :- y = 1."))

    PrettyPrinter.print(c.patterns.values)
  }

  test("directives") {
    val c = new Compiler

    c.compileRelationDecl(Parser.parse(Parser.relationDecl, ".decl A(a: number, b: symbol)"))
    c.compileRelationDecl(Parser.parse(Parser.relationDecl, ".decl B(a: float)"))

    c.compileDirective(Parser.parse(Parser.directive, ".input A"))
    c.compileDirective(Parser.parse(Parser.directive, ".output A"))
    c.compileDirective(Parser.parse(Parser.directive, ".printsize B"))
    c.compileDirective(Parser.parse(Parser.directive, ".limitsize B(n = 42)"))

    c.inputs.map(c.relationDecls.apply).foreach(PrettyPrinter.print)
    c.printSizes.map(c.relationDecls.apply).foreach(PrettyPrinter.print)
    c.limitSizes.foreach { case (name, n) =>
      println(PrettyPrinter.stringify(c.relationDecls(name)) + " -> " + n) }
  }

  test("TypeDeclSubtype") {
    val c = new Compiler

    c.compileTypeDecl(TypeDeclSubtype("Test", DeclaredType("TestSuper")))
    c.compileTypeDecl(TypeDeclSubtype("Test", DeclaredType("TestSuper2")))

    println(c.subTypes)
  }

  test("TypeDeclUnionTypeCorrect") {
    val c = new Compiler

    c.compileTypeDecl(TypeDeclUnion("Test", Seq(NumberType, NumberType)))
    assertFail(c.compileTypeDecl(TypeDeclUnion("Test", Seq(FloatType, NumberType))))

    println(c.unionTypes)
  }

  test("TypeDeclRecordType"){
    val c = new Compiler

    c.compileTypeDecl(TypeDeclRecord("Test", Seq(Attribute("test", FloatType), Attribute("test2", NumberType))))
    assertFail(c.compileTypeDecl(TypeDeclRecord("Test", Seq(Attribute("test", FloatType)))))

    println(c.recordTypes)
  }

  test("TypeDeclADT"){
    val c = new Compiler

    c.compileTypeDecl(TypeDeclADT("Test",
      Seq(ADTBranch("TestId", Seq(Attribute("TestAt1", NumberType), Attribute("TestAt2", FloatType))))))
    assertFail(c.compileTypeDecl(TypeDeclADT("Test",
      Seq(ADTBranch("TestId", Seq(Attribute("TestAt1", NumberType), Attribute("TestAt2", FloatType)))))))
    assertFail(c.compileTypeDecl(TypeDeclADT("Test2",
      Seq(ADTBranch("TestId", Seq(Attribute("TestAt1", NumberType), Attribute("TestAt2", FloatType)))))))
    assertFail(c.compileTypeDecl(TypeDeclADT("Test3",
      Seq(ADTBranch("TestBranchId",
        Seq(Attribute("TestAt1", NumberType), Attribute("TestAt2", FloatType))),
        ADTBranch("TestBranchId", Seq(Attribute("TestAt1", NumberType), Attribute("TestAt2", FloatType)))))))
    c.compileTypeDecl(TypeDeclADT("Test4",
      Seq(ADTBranch("TestBranchIdNew1",
        Seq(Attribute("TestAt1", NumberType), Attribute("TestAt2", FloatType))),
        ADTBranch("TestBranchIdNew2", Seq(Attribute("TestAt1", NumberType), Attribute("TestAt2", FloatType))))))

    println(c.algebraicDataTypes)
  }

  test("count aggregation") {
    val c = new Compiler

    val r = c.compileConstraint(Parser.parse(Parser.constraint, "x = count : Hello(y)"))
    println(r)
    PrettyPrinter.print(r)
  }
}
