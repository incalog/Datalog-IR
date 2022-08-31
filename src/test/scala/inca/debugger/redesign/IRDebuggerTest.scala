package inca.debugger.redesign

import inca.analyzedLangs.Exp
import inca.analyzedLangs.ExpLangTestAnalyses
import inca.backend.ir.Datalog
import inca.compiler.CompiledModule
import inca.compiler.Compiler
import inca.compiler.Options
import inca.debugger.table.ImmutableTable
import inca.debugger.ExamplePrograms._
import inca.debugger.ScalaValue
import inca.debugger.URIValue
import inca.debugger.Value
import inca.runtime.context.DataModel
import inca.runtime.context.QueryScope
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import inca.runtime.EnginePool
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Assertion
import truechange.EditScript

class IRDebuggerTest extends AnyFunSuite {

  def constructInput(edges: Seq[(Int, Int)]): DatabaseInput = {
    val inserts = edges.map { case (from, to) =>
      Tuples.staticArityFlatTupleOf(from, to)
    }.toSet
    DatabaseInput(EditScript(Seq()), Map("edge" -> inserts), Map())
  }

  def initDebugger(
      _module: Datalog.Module,
      dm: DataModel,
      dbInput: DatabaseInput
    ): Debugger = {
    val compiled = Compiler.compileGP(_module, dm, Options())
    val transformedModule = BlacklistTransformation.transformer(dm).transformModule(_module)
    val compiledTransformed = Compiler.compileGP(transformedModule, dm, Options())
    val runtime = initDatabaseRuntime(compiledTransformed, dbInput)
    val debugger = new IRDebugger(compiled.ir)
    debugger.setBottomUpRuntime(runtime)
    debugger
  }

  def initDebugger(
      _module: Datalog.Module,
      dm: DataModel,
      es: EditScript = EditScript(Seq())
    ): Debugger = {
    initDebugger(_module, dm, DatabaseInput(es, Map(), Map()))
  }

  def initDatabaseRuntime(compiled: CompiledModule, input: DatabaseInput): DatalogRuntime = {
    val scope = new QueryScope(compiled.dataModel)
    val (_engine, _database) =
      EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    _engine.delayUpdatePropagation(() => {
      _database.processDatabaseInput(input)
    })
    DatalogRuntime(_engine, _database, compiled)
  }

  def assertExpectedTable(
      debugger: Debugger,
      name: String,
      args: ImmutableTable[Value]
    ): Assertion = {
    val evalResult = debugger.callStack.top.asInstanceOf[EvaluationResult]
    val derived = evalResult.predResult
    val expected = debugger.state.readBottomUp(name, args)
    assertResult(expected)(derived)
  }
  def assertCurrentBody(debugger: Debugger, expected: ImmutableTable[Value]): Assertion = {
    assert(debugger.callStack.top.isInstanceOf[InRule])
    val evalResult = debugger.callStack.top.asInstanceOf[InRule]
    val derived = evalResult.current.ruleResult
    assertResult(expected)(derived)
  }

  def stepTillFinish(debugger: Debugger): Unit = {
    while (!debugger.isFinished) {
//      println(debugger.callStack.top)
//      println("=======================================")
      debugger.stepInto()
    }
  }

  val emptyDataModel: DataModel = new DataModel()
  val emptyInput: DatabaseInput = DatabaseInput.empty

  // Step into tests
  test("simple stepinto of single call") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel, emptyInput)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("one", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "one", args)
  }

  test("simple stepinto") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel, emptyInput)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "two", args)
  }

  test("test compare atoms") {
    val debugger = initDebugger(module(comparatorPattern), emptyDataModel, emptyInput)
    val args = ImmutableTable[Value](
      Seq("from"),
      Seq(Seq(ScalaValue(4)), Seq(ScalaValue(1)), Seq(ScalaValue(2)), Seq(ScalaValue(3)))
    )
    debugger.entry("edge", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "edge", args)
  }

  test("test has type atom") {
    val tree = Exp.Mul(Exp.IntegerLit(1), Exp.IntegerLit(2))
    val mulURI = tree.uri

    val debugger = initDebugger(module(ExpLangTestAnalyses.mulPattern), Exp.model, tree.loadEdits)

    val args = ImmutableTable[Value](Seq("mul"), Seq(Seq(URIValue(mulURI))))
    debugger.entry("mul", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "mul", args)
  }

  test("test two has type atoms join") {
    val tree = Exp.Mul(Exp.IntegerLit(1), Exp.IntegerLit(2))

    val debugger =
      initDebugger(module(ExpLangTestAnalyses.mulIntLitPattern), Exp.model, tree.loadEdits)

    val args = ImmutableTable[Value](Seq("mul"), Seq(Seq(URIValue(tree.uri))))
    debugger.entry("mulIntLit", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "mulIntLit", args)
  }

  test("test path atom") {
    val tree = Exp.Mul(Exp.Add(Exp.IntegerLit(1), Exp.IntegerLit(2)), Exp.IntegerLit(3))
    val mulURI = tree.uri
    val mulLhsURI = tree.lhs.uri

    val debugger = initDebugger(module(ExpLangTestAnalyses.lhsPattern), Exp.model, tree.loadEdits)

    val args =
      ImmutableTable[Value](Seq("exp"), Seq(Seq(URIValue(mulURI)), Seq(URIValue(mulLhsURI))))
    debugger.entry("lhs", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "lhs", args)
  }

  test("test path atom where trg is literal") {
    val tree = Exp.Add(Exp.IntegerLit(1), Exp.IntegerLit(2))
    val intLitLhs = tree.lhs.uri
    val intLitRhs = tree.rhs.uri

    val debugger = initDebugger(module(ExpLangTestAnalyses.intVal), Exp.model, tree.loadEdits)

    val args =
      ImmutableTable[Value](Seq("exp"), Seq(Seq(URIValue(intLitLhs)), Seq(URIValue(intLitRhs))))
    debugger.entry("intVal", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "intVal", args)
  }

  test("test call with literal as argument") {
    val tree = Exp.Let("y", Exp.IntegerLit(5), Exp.Let("x", Exp.IntegerLit(4), Exp.IntegerLit(9)))
    val debugger = initDebugger(
      module(ExpLangTestAnalyses.letBindingX, ExpLangTestAnalyses.boundIdOfLet),
      Exp.model,
      tree.loadEdits
    )

    val args = ImmutableTable.unit[Value]()
    debugger.entry("letBindingX", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "letBindingX", args)
  }

  test("test negative call") {
    val debugger = initDebugger(negationModule, emptyDataModel)
    val args = ImmutableTable[Value](Seq("x"), Seq(Seq(ScalaValue(1))))
    debugger.entry("nodesNotTwoHop", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "nodesNotTwoHop", args)
  }

  test("simple path step into") {
    val input = constructInput(Seq(1 -> 2, 2 -> 3, 3 -> 1))
    val debugger =
      initDebugger(module(pathPatternExt), new DataModel(), input)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "path", args)
//    assertResult(debugger.state.readBottomUp("path", args))(
//      debugger.state.readTopDown("path", args))
  }

  test("step into left recursive pattern") {
    val debugger = initDebugger(module(sevenEdgePattern, pathPatternLeftRecursive), emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into left recursive pattern with simple cyclic data") {
    val debugger =
      initDebugger(module(simpleCycleEdgePattern, pathPatternLeftRecursive), emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into left recursive pattern with simple cyclic data 2") {
    val debugger =
      initDebugger(module(simpleCycleEdgePattern2, pathPatternLeftRecursive), emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into left recursive pattern with cyclic data") {
    val debugger = initDebugger(module(cycleEdgePattern, pathPatternLeftRecursive), emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern 2") {
    val debugger = initDebugger(module(sevenEdgePattern, pathPatternSwitchBodies), emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern with simple cyclic data") {
    val debugger = initDebugger(module(simpleCycleEdgePattern, pathPattern), emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern with simple cyclic data 2") {
    val debugger = initDebugger(module(simpleCycleEdgePattern2, pathPattern), emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern with three hop cycle") {
    val debugger = initDebugger(module(threeHopCyclePattern, pathPattern), emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern with cyclic data") {
    val debugger = initDebugger(module(cycleEdgePattern, pathPattern), emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("test negative call of recursive pattern 1") {
    val debugger = initDebugger(
      module(sevenEdgePattern, nodePattern, pathPattern, notTargetOfPattern),
      emptyDataModel
    )
    val args = ImmutableTable[Value](Seq("n"), Seq(Seq(ScalaValue(1))))
    debugger.entry("notTargetOf", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "notTargetOf", args)
  }

  test("test negative call of recursive pattern 2") {
    val debugger = initDebugger(
      module(sevenEdgePattern, nodePattern, pathPattern, notTargetOfPattern),
      emptyDataModel
    )
    val args = ImmutableTable[Value](Seq("n"), Seq(Seq(ScalaValue(2))))
    debugger.entry("notTargetOf", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "notTargetOf", args)
  }

  val extEdgeInput: DatabaseInput = DatabaseInput(
    EditScript(Seq()),
    Map(
      "extEdge" -> Set(
        Tuples.flatTupleOf(1, 2),
        Tuples.flatTupleOf(2, 3),
        Tuples.flatTupleOf(3, 4),
        Tuples.flatTupleOf(4, 5),
        Tuples.flatTupleOf(5, 2))),
    Map()
  )

  test("ext call no arguments") {
    val debugger = initDebugger(module(extCallEdgePattern), emptyDataModel, extEdgeInput)
    val args = ImmutableTable.unit[Value]()
    debugger.entry("edge", args)
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "edge", args)
  }

  test("ext call with one argument bound") {
    val debugger = initDebugger(module(extCallEdgePattern), emptyDataModel, extEdgeInput)
    val args = ImmutableTable[Value](Seq("x"), Seq(Seq(ScalaValue(1)), Seq(ScalaValue(2))))
    debugger.entry("edge", args)
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "edge", args)
  }

  test("ext call with both arguments bound") {
    val debugger = initDebugger(module(extCallEdgePattern), emptyDataModel, extEdgeInput)
    val args = ImmutableTable[Value](Seq("y", "x"), Seq(Seq(ScalaValue(4), ScalaValue(3))))
    debugger.entry("edge", args)
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "edge", args)
  }

  // step over tests

  test("step over pattern") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    debugger.stepOver()
    debugger.stepOver()

    assert(debugger.isFinished)

    assertExpectedTable(debugger, "two", args)
  }

  test("step over non-rec call") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepOver() // step over edge call
    debugger.stepOver() // step over one call

    debugger.stepOver() // needed to step to pattern exit
    debugger.stepOver() // needed to exit pattern

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "two", args)
  }

  test("step over rec pattern (not part of scc)") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, sevenEdgePattern), emptyDataModel)
    val args = ImmutableTable.unit[Value]()
    debugger.entry("query", args)
    debugger.stepInto() // step into pattern
    debugger.stepInto() // step into rule
    debugger.stepOver() // step computed
    debugger.stepOver() // step over negated path call

    debugger.stepOver() // needed to step to pattern exit
    debugger.stepOver() // needed to exit pattern

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "query", args)
  }

  test("simple path step over recursive (depth 1)") {
    val input = constructInput(Seq(1 -> 2, 2 -> 3, 3 -> 1))
    val debugger =
      initDebugger(module(pathPatternExt), new DataModel(), input)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.state.insertBlacklist("path", args)
    debugger.entry("path", args)
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepOver()
    assertCurrentBody(
      debugger,
      ImmutableTable[Value](
        Seq("from", "temp", "to"),
        Seq(
          Seq(ScalaValue(1), ScalaValue(2), ScalaValue(1)),
          Seq(ScalaValue(1), ScalaValue(2), ScalaValue(3))
        ))
    )
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "path", args)
  }

  test("simple path step over recursive (depth 1) 2") {
    val input = constructInput(Seq(1 -> 2, 2 -> 3, 3 -> 2, 3 -> 1))
    val debugger =
      initDebugger(module(pathPatternExt), new DataModel(), input)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.state.insertBlacklist("path", args)
    debugger.entry("path", args)
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepOver()
    assertCurrentBody(
      debugger,
      ImmutableTable[Value](
        Seq("from", "temp", "to"),
        Seq(
          Seq(ScalaValue(1), ScalaValue(2), ScalaValue(1)),
          Seq(ScalaValue(1), ScalaValue(2), ScalaValue(3)),
          Seq(ScalaValue(1), ScalaValue(2), ScalaValue(2))
        )
      )
    )
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "path", args)
  }

  test("simple path step over recursive (depth 2)") {
    val input = constructInput(Seq(1 -> 2, 2 -> 3, 3 -> 2, 3 -> 1))
    val debugger =
      initDebugger(module(pathPatternExt), new DataModel(), input)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.state.insertBlacklist("path", args)
    debugger.entry("path", args)
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto() // path(2, ?) entry

    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepOver() // path(3, ?) call

    assertCurrentBody(
      debugger,
      ImmutableTable[Value](
        Seq("from", "temp", "to"),
        Seq(
          Seq(ScalaValue(2), ScalaValue(3), ScalaValue(1)),
          Seq(ScalaValue(2), ScalaValue(3), ScalaValue(2))
        )
      )
    )
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "path", args)
  }

  test("step over body") {
    val debugger = initDebugger(module(sevenEdgePattern), emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("edge", args)
    debugger.stepInto()
    debugger.stepOver() // step over body

    assertCurrentBody(
      debugger,
      ImmutableTable[Value](
        Seq("from", "to"),
        Seq(
          Seq(ScalaValue(1), ScalaValue(2))
        )
      )
    )
  test("step over rec pattern (part of scc)") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, sevenEdgePattern), emptyDataModel)
    val args = ImmutableTable.unit[Value]()
    debugger.entry("query", args)
    debugger.stepInto() // step into pattern
    debugger.stepInto() // step into body
    debugger.stepOver() // step over computed
    debugger.stepInto() // step into path call
    debugger.stepInto() // step into pattern
    debugger.stepOver() // step over first body
    debugger.stepInto() // step to edge call
    debugger.stepOver() // step over edge call
    debugger.stepOver() // step over recursive path call

    val expected = ImmutableTable[Value](
      Seq("from", "temp", "to"),
      Seq(
        Seq(ScalaValue(1), ScalaValue(2), ScalaValue(3)),
        Seq(ScalaValue(1), ScalaValue(2), ScalaValue(6)),
        Seq(ScalaValue(1), ScalaValue(2), ScalaValue(7)),
        Seq(ScalaValue(1), ScalaValue(4), ScalaValue(6)),
        Seq(ScalaValue(1), ScalaValue(4), ScalaValue(7))
      )
    )
    assertCurrentBody(debugger, expected)

    debugger.stepOver() // needed to step to pattern exit (of path call)
    debugger.stepOver() // needed to step to body exit
    debugger.stepOver() // needed to step to pattern exit (of notTargetof)
    debugger.stepOver() // pop pattern exit (of notTargetof)
    debugger.stepOver()
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "query", args)
  }
  }
}
