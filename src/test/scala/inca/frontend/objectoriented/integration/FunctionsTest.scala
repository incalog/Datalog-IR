package inca.frontend.objectoriented.integration

import inca.frontend.objectoriented.executor.ObjectExecutor
import inca.util.FileUtil.readFile
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.executor.ObjectExecutor.TypeCastException
import org.scalatest.Assertion

import scala.meta.{Term, XtensionQuasiquoteTerm}

case class ResultError(msg: String)

class FunctionsTest extends AnyFunSuite {

  val options: ObjectOptions = ObjectOptions()

  // type alias for tuple and set results
  // TODO: Replace this with actual classes ?
  type TupleResult[T] = Seq[T]
  object TupleResult {
    def apply(values: Any *): Seq[Any] = values
  }

  object UnitResult {
    def apply(): TupleResult[Any] = TupleResult()
  }

  type SetResult[T] = Set[T]
  object SetResult {
    def apply(values: Any*): Set[Any] = values.toSet
  }

  private def flatten(tup: TupleResult[Any]): TupleResult[Any] = tup.flatMap {
    case s: TupleResult[_] => flatten(s)
    case e => TupleResult(e)
  }

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
      case expectedSet: SetResult[_] =>
        if (result.size > expectedSet.size)
          Seq(ResultError(s"Expected set with size ${expectedSet.size}, but got ${result.size}."))
        else {
          result.flatMap { actual =>
            val anyErrorFree = expectedSet.exists(checkResult(_, Seq(actual)).isEmpty)
            if (!anyErrorFree)
              Seq(ResultError(s"Can not find $actual in result $expectedSet"))
            else
              Seq()
          }
        }

      // match datalog tuples
      case expectedTuple: TupleResult[_] =>
        // datalog flattens the output tuple => flatten the expected result as well
        val flattenTuple = flatten(expectedTuple)
        if (result.size > 1)
          Seq(ResultError(s"Expected single result, but got result set with size ${result.size}."))
        else if (result.head.size != flattenTuple.size)
          Seq(ResultError(s"Expected ${flattenTuple.size} result(s), but got ${result.head.size}."))
        else if (!result.head.zip(flattenTuple).forall { case (actual, expectedValue) => expectedValue.equals(actual) })
          Seq(ResultError(s"""Expectd ${flattenTuple.mkString("(", ", ", ")")}, but got ${result.head.mkString("(", ", ", ")")}"""))
        else
          Seq()

      case value =>
        checkResult(TupleResult(value), result)
    }
  }

  test("Base Examples") {
    val dir: String = "base/"
    performTest(dir+"Base1", "Base1$main", Seq(), 43)
    performTest(dir+"Base2", "Base2$main", Seq(), 43)
    performTest(dir+"Base3", "Base3$main", Seq(), 43)
  }

  test("Factorial Example") {
    performTest("Fact", "Factorial$main", Seq(q"5"), 120)
  }

  test("Fibonacci Example") {
    performTest("Fib", "Fibonacci$main", Seq(q"11"), 89)
  }

  test("Field Example") {
    val dir: String = "field/"
    performTest(dir+"FieldAccess", "Fraction$main", Seq(q"16", q"8"), 2)
    performTest(dir+"FieldAccessNested", "A$main", Seq(), 3)
    performTest(dir+"FieldDeclare", "A$main", Seq(), 3)
    performTest(dir+"FieldInheritance", "A$main", Seq(), 10)
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

  test("Object as param Example") {
    performTest("ParamObject", "A$main", Seq(), 1)
  }

  test("MethodInheritance Example") {
    performTest("MethodInheritance", "A$main", Seq(), 3)
  }

  test("BinaryTree Sum") {
    performTest("BinaryTree", "DefinedNode$main", Seq(), 20)
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
    val dir: String = "if/"
    // If Constant
    performTest(dir+"IfTrue", "IfTest$main", Seq(), true)
    performTest(dir+"IfFalse", "IfTest$main", Seq(), true)
    // If nested
    performTest(dir+"If", "IfTest$main", Seq(q"true", q"true"), 11)
    performTest(dir+"If", "IfTest$main", Seq(q"true", q"false"), 7)
    performTest(dir+"If", "IfTest$main", Seq(q"false", q"false"), 6)
    // Duplicate
    performTest(dir+"IfDuplicate", "IfTest$main", Seq(q"true", q"true"), 10)
  }

  test("Return Example") {
    val dir: String = "return/"
    performTest(dir+"Return", "ReturnTest$main", Seq(q"true"), 1)
    performTest(dir+"Return", "ReturnTest$main", Seq(q"false"), 2)
    performTest(dir+"ReturnTwice", "ReturnTest$main", Seq(), 1)
    performTest(dir+"ReturnImplicit", "ReturnTest$main", Seq(), 1)
    performTest(dir+"ReturnImplicitIf", "ReturnTest$main", Seq(q"true"), 1)
    performTest(dir+"ReturnImplicitUnit", "ReturnTest$main", Seq(q"true"), UnitResult())
    performTest(dir+"Unit", "A$main", Seq(), UnitResult())
  }

  test("Super Example") {
    performTest("Super", "A$main", Seq(), 10)
  }

  test("Tuple Example") {
    performTest("Tuple", "A$main", Seq(), TupleResult(true, TupleResult(true, true)))
  }

  test("Set Example") {
    val dir: String = "set/"
    performTest(dir+"SetConst", "A$main", Seq(), SetResult(1, 2, 3))
    performTest(dir+"SetConstVar", "A$main", Seq(), SetResult(1, 2, 3))
    performTest(dir+"SetMultiVar", "A$main", Seq(), SetResult(1, 2, 3))
    performTest(dir+"SetParam", "A$main", Seq(), SetResult(1, 2, 3))
    performTest(dir+"SetParamSquare", "A$main", Seq(), SetResult(1, 4, 9))
    performTest(dir+"Set", "A$main", Seq(), SetResult(1, 2, 3))
    performTest(dir+"SetFieldDeclare", "A$main", Seq(), SetResult(1, 2, 3))
    performTest(dir+"SetFieldSet", "A$main", Seq(), SetResult(1, 2, 3))
    performTest(dir+"SetTuple", "A$main", Seq(), SetResult(TupleResult(1, "A"), TupleResult(2, "B"), TupleResult(3, "C")))
    performTest(dir+"SetIntersection", "A$main", Seq(), SetResult(1, 3))
    performTest(dir+"SetUnion", "A$main", Seq(), SetResult(1, 2, 3, 4))
    performTest(dir+"SetUnionMixed", "A$main", Seq(), SetResult(1, 2, 3, 4))
    performTest(dir+"SetUnionIntersection", "A$main", Seq(), SetResult(1, 2, 3, 4))
    performTest(dir+"SetMethodNested", "A$main", Seq(), SetResult(1, 2, 3, 4))
    // Set Comprehension
    performTest(dir+"SetComprehension", "A$main", Seq(), SetResult(TupleResult(1, 3, 5), TupleResult(1, 4, 5)))
    performTest(dir+"SetComprehensionTuple", "A$main", Seq(), SetResult(TupleResult("A", 2), TupleResult("C", 2)))
  }

  /*test("Set Reduce") {
    performTest("SetReduce", "Num$main", Seq(), SetResult(TupleResult(1, 3, 5), TupleResult(1, 4, 5)))
  }*/

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