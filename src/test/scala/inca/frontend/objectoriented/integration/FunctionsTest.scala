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

case class ResultError(msg: String)

class FunctionsTest extends AnyFunSuite {

  val options: ObjectOptions = ObjectOptions()

  // TODO: Introduce result types such as SingleResult, TupleResult and SetResult. Convert the result type to the
  //  correct result subclass.
  private def performTest[O](file: String, main: String, input: Seq[Term], expectedResult: O): Seq[Assertion] = {
    val code = readFile(s"objectoriented/unittests/$file.oinca")
    val fun = ObjectExecutor.loadFunction(code, options)
    val result = fun.execute(main, input)
    fun.printAllMatches()

    if (result.res.isEmpty)
      Seq(assert(false, s"Expected a result, but got none."))
    else {
      checkResult(expectedResult, result.res).map(err => assert(false, err.msg))
    }
  }


  def checkResult[O](expectedResult: O, result: Seq[Seq[AnyRef]]): Seq[ResultError] = {
    expectedResult match {
      // match datalog sets
      case resultSet: Set[_] =>
        if (result.size > resultSet.size)
          Seq(ResultError(s"Expected set with size $resultSet.size, but got ${result.size}."))
        else {
          result.flatMap { actual =>
            val anyErrorFree = resultSet.filter(checkResult(_, Seq(actual)).isEmpty)
            if (anyErrorFree.isEmpty)
              Seq(ResultError(s"Can not find $actual in result $resultSet"))
            else
              Seq()
          }
        }

      // match datalog tuples
      // TODO: Support nested tuples
      case resultTuple: Seq[_] =>
        if (result.size > 1)
          Seq(ResultError(s"Expected single result, but got result set with size ${result.size}."))
        else if (result.head.size != resultTuple.size)
          Seq(ResultError(s"Expected ${resultTuple.size} result(s), but got ${result.head.size}."))
        else if (!result.head.zip(resultTuple).forall { case (actual, expectedValue) => expectedValue.equals(actual) })
          Seq(ResultError(s"""Expectd ${resultTuple.mkString("(", ", ", ")")}, but got ${result.head.mkString("(", ", ", ")")}"""))
        else
          Seq()

      case value =>
        // Perform a tuple check with one element
        checkResult(Seq(value), result)
    }
  }

  test("Base 1 Example") {
    performTest("Base1", "Base1$main", Seq(), 43)
  }

  test("Base 2 Example") {
    performTest("Base2", "Base2$main", Seq(), 43)
  }

  test("Base 3 Example") {
    performTest("Base3", "Base3$main", Seq(), 43)
  }

  test("Factorial Example") {
    performTest("Fact", "Factorial$main", Seq(q"5"), 120)
  }

  test("Fibonacci Example") {
    performTest("Fib", "Fibonacci$main", Seq(q"11"), 89)
  }

  test("FieldAccess Example") {
    performTest("FieldAccess", "Fraction$main", Seq(q"16", q"8"), 2)
  }

  test("FieldAccessNested Example") {
    performTest("FieldAccessNested", "A$main", Seq(), 3)
  }

  test("FieldDeclare Example") {
    performTest("FieldDeclare", "A$main", Seq(), 3)
  }

  test("FieldInheritance Example") {
    performTest("FieldInheritance", "A$main", Seq(), 10)
  }

  test("Constructor Example") {
    performTest("Constructor", "Fraction$main", Seq(q"16", q"8", q"1"), 3)
  }

  test("Null") {
    performTest("Null", "NullTest$main", Seq(), true)
  }

  test("Equals") {
    performTest("Equals", "EqualsTest$main", Seq(), true)
  }

  test("InstanceOf Example") {
    performTest("InstanceOf", "A$main", Seq(), true)
  }

  test("TypeCast Example") {
    performTest("TypeCast", "A$main", Seq(), true)
  }

  test("TypeCastFail Example") {
    val caught = intercept[TypeCastException] {
      performTest("TypeCastFail", "A$main", Seq(), Seq())
    }
    assert(caught.typ == "B")
    assert(caught.obj.typ == "A")
  }

  test("DynamicDispatch Example") {
    performTest("DynamicDispatch", "A$main", Seq(), "BBC")
  }

  test("MethodInheritance Example") {
    performTest("MethodInheritance", "A$main", Seq(), 3)
  }

  test("BinaryTree Sum") {
    performTest("BinaryTree3", "DefinedNode$main", Seq(), 20)
  }

  test("Plus Example") {
    performTest("Plus", "Nat$main", Seq(), 5)
  }

  test("Mutability Example") {
    performTest("Mutability", "A$main", Seq(), true)
  }

  test("VarAssignment Example") {
    performTest("VarAssignment", "A$main", Seq(q"3"), true)
  }

  test("If Example") {
    // If Constant
    performTest("IfTrue", "IfTest$main", Seq(), true)
    performTest("IfFalse", "IfTest$main", Seq(), true)

    // If nested
    performTest("If", "IfTest$main", Seq(q"true", q"true"), 11)
    performTest("If", "IfTest$main", Seq(q"true", q"false"), 7)
    performTest("If", "IfTest$main", Seq(q"false", q"false"), 6)

    // Duplicate
    performTest("IfDuplicate", "IfTest$main", Seq(q"true", q"true"), 10)
  }

  test("Return Example") {
    performTest("Return", "ReturnTest$main", Seq(q"true"), 1)
    performTest("Return", "ReturnTest$main", Seq(q"false"), 2)
  }

  test("Super Example") {
    performTest("Super", "A$main", Seq(), 10)
  }

  test("Tuple Example") {
    performTest("Tuple", "A$main", Seq(), Seq(true, true, true))
  }

  test("Set Example") {
    performTest("Set", "A$main", Seq(), Set(1, 2, 3))
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