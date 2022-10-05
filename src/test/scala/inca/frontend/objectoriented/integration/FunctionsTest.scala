package inca.frontend.objectoriented.integration

import inca.backend.ir.util.printer.DatalogPrinter
import inca.frontend.objectoriented.executor.ObjectExecutor
import inca.util.FileUtil.readFile
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.executor.ObjectExecutor.TypeCastException
import org.apache.log4j.Level
import org.scalatest.Assertion

import scala.meta.{Term, XtensionQuasiquoteTerm}

class FunctionsTest extends AnyFunSuite {

  val options: ObjectOptions = ObjectOptions()

  private def performSingleOutputValueTest[O](file: String, main: String, input: Seq[Term], expectedResult: O): Assertion = {
    val code = readFile(s"objectoriented/unittests/$file.oinca")
    val fun = ObjectExecutor.loadFunction(code, options)
    val result = fun.execute(main, input)
    fun.printAllMatches()
    if (result.res.size > 1)
      assert(false, s"Expected one result, but got ${result.res.size}.")
    assertResult(expectedResult)(result.res.head.head)
  }

  test("Base 1 Example") {
    performSingleOutputValueTest("Base1", "Base1$main", Seq(), 43)
  }

  test("Base 2 Example") {
    performSingleOutputValueTest("Base2", "Base2$main", Seq(), 43)
  }

  test("Base 3 Example") {
    performSingleOutputValueTest("Base3", "Base3$main", Seq(), 43)
  }

  test("Factorial Example") {
    performSingleOutputValueTest("Fact", "Factorial$main", Seq(q"5"), 120)
  }

  test("Fibonacci Example") {
    performSingleOutputValueTest("Fib", "Fibonacci$main", Seq(q"11"), 89)
  }

  test("FieldAccess Example") {
    performSingleOutputValueTest("FieldAccess", "Fraction$main", Seq(q"16", q"8"), 2)
  }

  test("FieldAccessNested Example") {
    performSingleOutputValueTest("FieldAccessNested", "A$main", Seq(), 3)
  }

  test("FieldDeclare Example") {
    performSingleOutputValueTest("FieldDeclare", "A$main", Seq(), 3)
  }

  test("FieldInheritance Example") {
    performSingleOutputValueTest("FieldInheritance", "A$main", Seq(), 10)
  }

  test("Constructor Example") {
    performSingleOutputValueTest("Constructor", "Fraction$main", Seq(q"16", q"8", q"1"), 3)
  }

  test("Null") {
    performSingleOutputValueTest("Null", "NullTest$main", Seq(), true)
  }

  test("Equals") {
    performSingleOutputValueTest("Equals", "EqualsTest$main", Seq(), true)
  }

  test("InstanceOf Example") {
    performSingleOutputValueTest("InstanceOf", "A$main", Seq(), true)
  }

  test("TypeCast Example") {
    performSingleOutputValueTest("TypeCast", "A$main", Seq(), true)
  }

  test("TypeCastFail Example") {
    val caught = intercept[TypeCastException] {
      performSingleOutputValueTest("TypeCastFail", "A$main", Seq(), Seq())
    }
    assert(caught.typ == "B")
    assert(caught.obj.typ == "A")
  }

  test("DynamicDispatch Example") {
    performSingleOutputValueTest("DynamicDispatch", "A$main", Seq(), "BBC")
  }

  test("MethodInheritance Example") {
    performSingleOutputValueTest("MethodInheritance", "A$main", Seq(), 3)
  }

  test("BinaryTree Sum") {
    performSingleOutputValueTest("BinaryTree3", "DefinedNode$main", Seq(), 20)
  }

  test("Plus Example") {
    performSingleOutputValueTest("Plus", "Nat$main", Seq(), 5)
  }

  test("Mutability Example") {
    performSingleOutputValueTest("Mutability", "A$main", Seq(), true)
  }

  test("VarAssignment Example") {
    performSingleOutputValueTest("VarAssignment", "A$main", Seq(q"3"), true)
  }

  test("If Example") {
    // If Constant
    performSingleOutputValueTest("IfTrue", "IfTest$main", Seq(), true)
    performSingleOutputValueTest("IfFalse", "IfTest$main", Seq(), true)

    // If nested
    performSingleOutputValueTest("If", "IfTest$main", Seq(q"true", q"true"), 11)
    performSingleOutputValueTest("If", "IfTest$main", Seq(q"true", q"false"), 7)
    performSingleOutputValueTest("If", "IfTest$main", Seq(q"false", q"false"), 6)

    // Duplicate
    performSingleOutputValueTest("IfDuplicate", "IfTest$main", Seq(q"true", q"true"), 10)
  }

  test("Return Example") {
    performSingleOutputValueTest("Return", "ReturnTest$main", Seq(q"true"), 1)
    performSingleOutputValueTest("Return", "ReturnTest$main", Seq(q"false"), 2)
  }

  test("Super Example") {
    performSingleOutputValueTest("Super", "A$main", Seq(), 10)
  }

  /*test("Generate example") {
    import inca.backend.optimize._
    import inca.compiler.Compiler
    import inca.backend.analyze.DependencyGraph
    import inca.backend.ir.util.printer.DatalogPrinter
    import inca.runtime.context.QueryScope
    import inca.runtime.EnginePool
    import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
    import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
    import inca.backend.transform.magic.demand._
    import inca.backend.transform.objectoriented._

    /*import org.eclipse.viatra.query.runtime.util.ViatraQueryLoggingUtil
    ViatraQueryLoggingUtil.setupConsoleAppenderForDefaultLogger()
    ViatraQueryLoggingUtil.getDefaultLogger.setLevel(Level.ALL)*/

    val result = Compiler.compileObject("module Test", ObjectOptions(Seq(
      /*EliminateNonproductiveRelations,
      InlineSimpleRelations,
      ConstantPropagation,
      EliminateAliases,
      EvalFusion,
      InferVarTypes,
      FoldConstantAtoms,
      EliminateNonproductiveRelations*/
    ), Seq(
      //AllocTransformation,
      //FieldTransformation,
      DeriveDemandPatterns,
      DemandTransformation
    )))

    /*val graph = new DependencyGraph(result.transformed)
    println("Dependency graph")
    println(graph.toGraphViz)*/

    /*println()
    println("DatalogPrinter")
    println(DatalogPrinter.prettyModule(result.transformed)(verbose = true))
    println()*/

    val patterns = result.optimized.pats.map(_.name)
    val specs = patterns.map(result.psystemModule.patterns(_)()) // Nat$main // "Succ" for all Succ instances
    val scope = new QueryScope(result.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    val matcher = specs.map(engine.getMatcher(_))
    feed.insert(DemandTransformation.demandPatternExtensionalPrefix + "main", Tuples.flatTupleOf())
    matcher.zip(patterns).foreach(m => println(m._2 + ": " + m._1.getAllMatches.toArray.mkString(", ")))

  }*/
}