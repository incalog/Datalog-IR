package inca.frontend.souffle.compiler

import inca.backend.ir.Datalog
import inca.frontend.souffle.Syntax._
import inca.frontend.souffle.executor.SouffleExecutor
import inca.frontend.souffle.executor.SouffleExecutor.{Loaded, Outputs, Results}
import inca.frontend.souffle.{Parser, PrettyPrinter, compiler}
import org.scalatest.funsuite.AnyFunSuite

// TODO use assertions instead of printing tests
class CompilerTest extends AnyFunSuite {

  def assertFail(f: => Any): Unit = {
    try f
    catch {
      case _: AssertionError =>
      case e: Exception => println("Expected exception: " + e)
      case e: Throwable => assert(0 == 1, e)
    }
  }

  def assertOutputTuples[A](relation: String)(toBe: Set[Seq[A]])(implicit outputs: Outputs): Unit = {
    assert(outputs(relation).res.map(_.toSeq).toSet == toBe)
  }

  def assertOutput[A](relation: String, column: Int = 0)(toBe: Set[A])(implicit outputs: Outputs): Unit = {
    assert(outputs(relation).res.map(_(column)).toSet == toBe)
  }

  def printOutputs(implicit outputs: Outputs, loaded: Loaded): Unit = {
    for ((k, v) <- outputs) {
      val args = loaded.compiled.ir.pats.find(_.name == k)
        .get.params.map(PrettyPrinter.stringify).mkString(", ")

      println(s"$k($args)")

      v.res.foreach(x => println(x.mkString(", ")))
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
    val program =
      """ .decl A(x: float)
        | A(5.0). A(4.2).
        |
        | .output A
        |""".stripMargin

    implicit val loaded: Loaded = SouffleExecutor.loadFunction(program)
    implicit val outputs: Outputs = loaded.execute("")

    printOutputs
    assertOutput("A")(Set(5.0f, 4.2f))
  }

  test("alias") {
    val c = new compiler.Compiler()

    c.compileArgument(Parser.parse(Parser.argumentAtom, "as(x, number)"))

    println(c.boundArguments)
    c.boundArguments.map(PrettyPrinter.print)
  }

  test("factFail") {
    val c = new compiler.Compiler

    c.compileRelationDecl(Parser.parse(Parser.relationDecl, ".decl A(x: number, y: number)"))
    assertFail(c.compileFact(Parser.parse(Parser.fact, "A(x, 0).")))

    c.compileRelationDecl(Parser.parse(Parser.relationDecl, ".decl B(x: number)"))
    assertFail(c.compileFact(Parser.parse(Parser.fact, "B(1, 2).")))
  }

  test("rule") {
    val program =
      """ .decl A(x: number, y: number)
        |
        | A(x, y) :- x = 0, y = 1.
        | A(x, y) :- x = 2, (y = 3; y = 4).
        | A(5, y) :- y = 6.
        |
        | .output A
        |""".stripMargin

    implicit val loaded: Loaded = SouffleExecutor.loadFunction(program)
    implicit val outputs: Outputs = loaded.execute("")

    printOutputs
    assertOutputTuples("A")(Set(Seq(0, 1), Seq(2, 3), Seq(2, 4), Seq(5, 6)))
  }

  test("strings") {
    val program =
      """ .decl A(x: symbol, y: symbol)
        | .decl B(z: symbol)
        |
        | A("Hello", "World").
        | A("Boogie", "Woogie").
        |
        | B(z) :- A(x, y), z = cat(x, y).
        |
        | .decl C(z: symbol, l: number)
        | C(z, l) :- B(z), l = strlen(z).
        |
        | .output B
        | .output C
        |""".stripMargin

    implicit val loaded: Loaded = SouffleExecutor.loadFunction(program)
    implicit val outputs: Outputs = loaded.execute("")

    printOutputs
    assertOutput("B")(Set("HelloWorld", "BoogieWoogie"))
    assertOutput("C", 1)(Set(12, 10))
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

  test("TypeDeclRecordType") {
    val c = new Compiler

    c.compileTypeDecl(TypeDeclRecord("Test", Seq(Attribute("test", FloatType), Attribute("test2", NumberType))))
    assertFail(c.compileTypeDecl(TypeDeclRecord("Test", Seq(Attribute("test", FloatType)))))

    println(c.recordTypes)
  }

  test("TypeDeclADT") {
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

  test("argument intrinsic functor") {
    val c = new Compiler

    c.compileArgument(Parser.parse(Parser.argumentAtom, "ord(a)"))
    c.compileArgument(Parser.parse(Parser.argumentAtom, "ord(\"some text\")"))
    c.compileArgument(Parser.parse(Parser.argumentAtom, "to_float(\"123\")"))
    c.compileArgument(Parser.parse(Parser.argumentAtom, "to_number(\"13\")"))
    println(c.boundArguments)
    c.boundArguments.map(PrettyPrinter.print)
  }

  test("argument unary operation") {
    val c = new Compiler

    c.compileArgument(ArgumentUnOp(UnOpMinus, ArgumentConstant(ConstantNumber(42))))
    println(c.boundArguments)
    c.boundArguments.map(PrettyPrinter.print)
  }

  test("argument binary operation") {
    val program =
      """ .decl A(x: number, y: number)
        | .decl B(z: number)
        | .decl C(z: number)
        |
        | A(1, 2). A(5, 9).
        |
        | B(z) :- A(x, y), z = x + y.
        | C(z) :- A(x, y), z = x * y.
        |
        | .output B
        | .output C
        |""".stripMargin

    implicit val loaded: Loaded = SouffleExecutor.loadFunction(program)
    implicit val outputs: Outputs = loaded.execute("")

    printOutputs
    assertOutput("B")(Set(3, 14))
    assertOutput("C")(Set(2, 45))
  }

  test("argument list") {
    val c = new Compiler

    c.compileArgument(ArgumentList(Seq(ArgumentConstant(ConstantNumber(1)), ArgumentConstant(ConstantNumber(2)))))
    println(c.boundArgumentList)
    c.boundArgumentList.values.map(PrettyPrinter.print)
  }

  test("aggregator with disjunction") {
    val c = new Compiler

    c.compileRelationDecl(RelationDecl("A", Seq(Attribute("x", NumberType))))
    c.compileRelationDecl(RelationDecl("B", Seq(Attribute("x", NumberType))))
    c.compileRelationDecl(RelationDecl("C", Seq(Attribute("x", NumberType))))

    val compiledMax = c.compileConstraint(ConstraintCmp(
      ConstraintCmpOp.Eq,
      ArgumentVariable("y"),
      ArgumentAggregator(AggregatorMax(
        ArgumentVariable("x"),
        AggregatorConditionDisjunction(TermDisjunction(Seq(TermConjunction(Seq(
          TermAtom(Atom("A", Seq(ArgumentVariable("x")))),
          TermAtom(Atom("B", Seq(ArgumentVariable("x")))),
          TermAtom(Atom("C", Seq(ArgumentVariable("x")))),
        )))))
      )))
    )

    val compiledMin = c.compileConstraint(ConstraintCmp(
      ConstraintCmpOp.Eq,
      ArgumentVariable("y"),
      ArgumentAggregator(AggregatorMin(
        ArgumentVariable("x"),
        AggregatorConditionDisjunction(TermDisjunction(Seq(TermConjunction(Seq(
          TermAtom(Atom("A", Seq(ArgumentVariable("x")))),
          TermAtom(Atom("B", Seq(ArgumentVariable("x")))),
        )))))
      )))
    )

    val compiledSum = c.compileConstraint(ConstraintCmp(
      ConstraintCmpOp.Eq,
      ArgumentVariable("y"),
      ArgumentAggregator(AggregatorSum(
        ArgumentVariable("x"),
        AggregatorConditionDisjunction(TermDisjunction(Seq(TermConjunction(Seq(
          TermAtom(Atom("A", Seq(ArgumentVariable("x")))),
          TermAtom(Atom("B", Seq(ArgumentVariable("x")))),
        )))))
      )))
    )

    println(c.relationDecls)
    println(c.patterns)

    c.relationDecls.values.foreach(PrettyPrinter.print)
    c.patterns.values.foreach(PrettyPrinter.print)

    println(compiledMax)
    PrettyPrinter.print(compiledMax)
    println(compiledMin)
    PrettyPrinter.print(compiledMin)
    println(compiledSum)
    PrettyPrinter.print(compiledSum)
  }

  test("souffle executor example") {
    val prog2 =
      s""".decl edge(x: number, y: number)
         |edge(1, 2).
         |edge(2, 3).
         |edge(3, 4).
         |edge(4, 2).
         |
         |.decl path(x: number, y: number)
         |.output path
         |.printsize path
         |path(x, y) :- edge(x, y).
         |path(x, y) :- edge(x, z), path(z, y).
         |""".stripMargin
    val loaded = SouffleExecutor.loadFunction(prog2)
    val outputs = loaded.execute("")

    println(outputs)
  }

  test("test input directive") {
    val prog =
      s""".decl edge(x: number, y: number)
         |.input edge(IO=file, filename="edge.facts", delimiter=",")
         |
         |.decl path(x: number, y: number)
         |.output path
         |.printsize path
         |path(x, y) :- edge(x, y).
         |path(x, y) :- edge(x, z), path(z, y).
         |""".stripMargin
    val loaded = SouffleExecutor.loadFunction(prog)
    val outputs = loaded.execute("new-souffle-frontend/testdata/path")

    for ((k, v) <- outputs) {
      println(s"Outputs for '$k':")
      v.res.foreach(x => println(x.mkString(", ")))
    }
  }

  test("advanced facts") {
    val program =
      s""" .decl A(x: float)
         | A(3 + 4).
         | A(42).
         | A(-42).
         |
         | .decl B(x: float)
         | B(range(-1, 4, 1.5)).
         |
         | .decl C(x: float)
         | C(range(3, 0)).
         |
         | .output A
         | .output B
         | .output C
         | .printsize A
         | .printsize B
         | .printsize C
         |""".stripMargin

    implicit val loaded: Loaded = SouffleExecutor.loadFunction(program)
    implicit val outputs: Outputs = loaded.execute("new-souffle-frontend/testdata/path")

    printOutputs
    assertOutput("A")(Set(7, 42, -42))
    assertOutput("B")(Set(-1, 0.5, 2, 3.5))
    assertOutput("C")(Set(3, 2, 1))
  }

  test("aggregation") {
    val program =
      s""" .decl A(x: number)
         | A(1). A(2). A(3).
         |
         | .decl B(y: number)
         | B(sum x : A(x)).
         |
         | .decl C(y: number)
         | C(count : A(_)).
         |
         | .decl D(y: number)
         | D(count : A(2)).
         |
         | .output A
         | .output B
         | .output C
         | .output D
         |""".stripMargin

    implicit val loaded: Loaded = SouffleExecutor.loadFunction(program)
    implicit val outputs: Outputs = loaded.execute("")

    printOutputs
    assertOutput("A")(Set(1, 2, 3))
    assertOutput("B")(Set(6))
    assertOutput("C")(Set(3))
    assertOutput("D")(Set(1))
  }
}
