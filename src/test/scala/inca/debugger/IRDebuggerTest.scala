package inca.debugger

import inca.analyzedLangs.Exp
import inca.analyzedLangs.ExpLangTestAnalyses
import inca.backend.ir.Datalog
import inca.compiler.CompiledModule
import inca.compiler.Compiler
import inca.compiler.Options
import inca.debugger.ExamplePrograms._
import inca.debugger.ScalaValue
import inca.debugger.URIValue
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
    val debugger = new InitializingIRDebugger(compiled, dbInput)
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

  def assertExpectedTable(debugger: Debugger, name: String, args: ValueTable): Assertion = {
    val derived = debugger.queryStack.top.asInstanceOf[QueryResult].t
    val expected = debugger.state.readBottomUp(name, args)
    println(s"Derived  $derived")
    println(s"Expected $expected")
    assertResult(expected)(derived)
  }

  def assertCurrentBody(debugger: Debugger, expected: ValueTable): Assertion = {
    assert(debugger.queryStack.top.isInstanceOf[Subquery])
    val sup = debugger.queryStack.top.asInstanceOf[Subquery].supplementary
    assertResult(expected)(sup)
  }

  def assertAtomResult(debugger: Debugger, expected: ValueTable): Assertion = {
    assert(debugger.queryStack.top.isInstanceOf[Subquery])
    val bodies = debugger.queryStack.top.asInstanceOf[Subquery].bodies
    assert(bodies.nonEmpty)
    assert(bodies.head.isInstanceOf[Rule])
    val atoms = bodies.head.asInstanceOf[Rule].atoms
    assert(atoms.nonEmpty)
    assert(atoms.head.isInstanceOf[AtomResult])
    val atomResult = atoms.head.asInstanceOf[AtomResult].t
    assertResult(expected)(atomResult)
  }

  def assertQueryResult(
      debugger: Debugger,
      name: String,
      expected: ValueTable
    ): Assertion = {
    val derived = debugger.queryStack.top.asInstanceOf[QueryResult].t
    assertResult(expected)(derived)
  }

  def edgeTable(columns: Seq[String], edges: (Int, Int)*): ValueTable = {
    val entries = edges.map { case (x, y) => Seq(ScalaValue(x), ScalaValue(y)) }
    ValueTable(columns, entries)
  }

  def stepTillFinish(debugger: Debugger): Unit = {
    while (!debugger.isFinished) {
      debugger.stepInto()
    }
  }

  val emptyDataModel: DataModel = new DataModel()
  val emptyInput: DatabaseInput = DatabaseInput.empty

  // Step into tests
  test("simple stepinto of single call") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel, emptyInput)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("one", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "one", args)
  }

  test("simple stepinto") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel, emptyInput)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "two", args)
  }

  test("test compare atoms") {
    val debugger = initDebugger(module(comparatorPattern), emptyDataModel, emptyInput)
    val args = ValueTable(
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

    val args = ValueTable(Seq("mul"), Seq(Seq(URIValue(mulURI))))
    debugger.entry("mul", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "mul", args)
  }

  test("test two has type atoms join") {
    val tree = Exp.Mul(Exp.IntegerLit(1), Exp.IntegerLit(2))

    val debugger =
      initDebugger(module(ExpLangTestAnalyses.mulIntLitPattern), Exp.model, tree.loadEdits)

    val args = ValueTable(Seq("mul"), Seq(Seq(URIValue(tree.uri))))
    debugger.entry("mulIntLit", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "mulIntLit", args)
  }

  test("test path atom") {
    val tree = Exp.Mul(Exp.Add(Exp.IntegerLit(1), Exp.IntegerLit(2)), Exp.IntegerLit(3))
    val mulURI = tree.uri
    val mulLhsURI = tree.lhs.uri

    val debugger = initDebugger(module(ExpLangTestAnalyses.lhsPattern), Exp.model, tree.loadEdits)

    val args = ValueTable(Seq("exp"), Seq(Seq(URIValue(mulURI)), Seq(URIValue(mulLhsURI))))
    debugger.entry("lhs", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "lhs", args)
  }

  test("test path atom where trg is literal") {
    val tree = Exp.Add(Exp.IntegerLit(1), Exp.IntegerLit(2))
    val intLitLhs = tree.lhs.uri
    val intLitRhs = tree.rhs.uri

    val debugger = initDebugger(module(ExpLangTestAnalyses.intVal), Exp.model, tree.loadEdits)

    val args = ValueTable(Seq("exp"), Seq(Seq(URIValue(intLitLhs)), Seq(URIValue(intLitRhs))))
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

    val args = ValueTable.unit()
    debugger.entry("letBindingX", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "letBindingX", args)
  }

  test("test negative call") {
    val debugger = initDebugger(negationModule, emptyDataModel)
    val args = ValueTable(Seq("x"), Seq(Seq(ScalaValue(1))))
    debugger.entry("nodesNotTwoHop", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "nodesNotTwoHop", args)
  }

  test("simple path step into") {
    val input = constructInput(Seq(1 -> 2, 2 -> 3, 3 -> 1))
    val debugger =
      initDebugger(module(pathPatternExt), new DataModel(), input)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into left recursive pattern") {
    val debugger = initDebugger(module(sevenEdgePattern, pathPatternLeftRecursive), emptyDataModel)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into left recursive pattern with simple cyclic data") {
    val debugger =
      initDebugger(module(simpleCycleEdgePattern, pathPatternLeftRecursive), emptyDataModel)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into left recursive pattern with simple cyclic data 2") {
    val debugger =
      initDebugger(module(simpleCycleEdgePattern2, pathPatternLeftRecursive), emptyDataModel)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into left recursive pattern with cyclic data") {
    val debugger = initDebugger(module(cycleEdgePattern, pathPatternLeftRecursive), emptyDataModel)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern 2") {
    val debugger = initDebugger(module(sevenEdgePattern, pathPatternSwitchBodies), emptyDataModel)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  // TODO fix
  test("step into recursive pattern with simple cyclic data") {
    val debugger = initDebugger(module(simpleCycleEdgePattern, pathPattern), emptyDataModel)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern with simple cyclic data 2") {
    val debugger = initDebugger(module(simpleCycleEdgePattern2, pathPattern), emptyDataModel)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern with three hop cycle") {
    val debugger = initDebugger(module(threeHopCyclePattern, pathPattern), emptyDataModel)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern with cyclic data") {
    val debugger = initDebugger(module(cycleEdgePattern, pathPattern), emptyDataModel)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
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
    val args = ValueTable(Seq("n"), Seq(Seq(ScalaValue(1))))
    debugger.entry("notTargetOf", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "notTargetOf", args)
  }

  test("test negative call of recursive pattern 2") {
    val debugger = initDebugger(
      module(sevenEdgePattern, nodePattern, pathPattern, notTargetOfPattern),
      emptyDataModel
    )
    val args = ValueTable(Seq("n"), Seq(Seq(ScalaValue(2))))
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
    val args = ValueTable.unit()
    debugger.entry("edge", args)
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "edge", args)
  }

  test("ext call with one argument bound") {
    val debugger = initDebugger(module(extCallEdgePattern), emptyDataModel, extEdgeInput)
    val args = ValueTable(Seq("x"), Seq(Seq(ScalaValue(1)), Seq(ScalaValue(2))))
    debugger.entry("edge", args)
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "edge", args)
  }

  test("ext call with both arguments bound") {
    val debugger = initDebugger(module(extCallEdgePattern), emptyDataModel, extEdgeInput)
    val args = ValueTable(Seq("y", "x"), Seq(Seq(ScalaValue(4), ScalaValue(3))))
    debugger.entry("edge", args)
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "edge", args)
  }

  // step over tests

  test("step over non-rec call") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    debugger.stepOver() // step over edge call
    assertAtomResult(
      debugger,
      edgeTable(Seq("from", "temp"), 1 -> 2, 1 -> 4, 1 -> 5)
    )
    debugger.stepOver() // rule merge
    debugger.stepOver() // step over one call
    assertAtomResult(
      debugger,
      edgeTable(Seq("temp", "to"), 2 -> 3, 2 -> 6, 4 -> 6)
    )

    stepTillFinish(debugger)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "two", args)
  }

  test("step over rec pattern (not part of scc)") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, sevenEdgePattern), emptyDataModel)
    val args = ValueTable.unit()
    debugger.entry("query", args)
    debugger.stepInto() // step over computed
    debugger.stepInto() // rule merge
    debugger.stepOver() // step over path
    assertAtomResult(
      debugger,
      edgeTable(Seq("from", "to"), 1 -> 2, 1 -> 3, 1 -> 4, 1 -> 5, 1 -> 6, 1 -> 7)
    )
    stepTillFinish(debugger)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "query", args)
  }

  test("simple path step over recursive (depth 1)") {
    val input = constructInput(Seq(1 -> 2, 2 -> 3, 3 -> 1))
    val debugger = initDebugger(module(pathPatternExt), new DataModel(), input)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    debugger.stepInto() // ext call
    debugger.stepInto() // rule merge
    debugger.stepInto() // rule result
    debugger.stepInto() // query union
    debugger.stepInto() // ext call
    debugger.stepInto() // rule merge
    debugger.stepOver() // path call
    assertAtomResult(
      debugger,
      edgeTable(Seq("temp", "to"), 2 -> 1, 2 -> 3)
    )
    stepTillFinish(debugger)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("simple path step over recursive (depth 1) 2") {
    val input = constructInput(Seq(1 -> 2, 2 -> 3, 3 -> 2, 3 -> 1))
    val debugger = initDebugger(module(pathPatternExt), new DataModel(), input)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    debugger.stepInto() // ext call
    debugger.stepInto() // rule merge
    debugger.stepInto() // rule result
    debugger.stepInto() // query union
    debugger.stepInto() // ext call
    debugger.stepInto() // rule merge
    debugger.stepOver() // path call
    assertAtomResult(
      debugger,
      edgeTable(Seq("temp", "to"), 2 -> 1, 2 -> 3, 2 -> 2)
    )
    stepTillFinish(debugger)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("simple path step over recursive (depth 2)") {
    val input = constructInput(Seq(1 -> 2, 2 -> 3, 3 -> 1))
    val debugger = initDebugger(module(pathPatternExt), new DataModel(), input)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    debugger.stepInto() // ext call
    debugger.stepInto() // rule merge
    debugger.stepInto() // rule result
    debugger.stepInto() // query union
    debugger.stepInto() // ext call
    debugger.stepInto() // rule merge
    debugger.stepInto() // path call
    debugger.stepInto() // ext call
    debugger.stepInto() // rule merge
    debugger.stepInto() // rule result
    debugger.stepInto() // query union
    debugger.stepInto() // ext call
    debugger.stepInto() // rule merge
    debugger.stepOver() // path call
    assertAtomResult(
      debugger,
      edgeTable(Seq("temp", "to"), 3 -> 1)
    )
    stepTillFinish(debugger)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step over rec pattern (part of scc)") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, sevenEdgePattern), emptyDataModel)
    val args = ValueTable.unit()
    debugger.entry("query", args)
    debugger.stepInto() // computed
    debugger.stepInto() // rule merge
    debugger.stepInto() // path call
    debugger.stepOver() // over edge call
    debugger.stepInto() // rule merge
    debugger.stepInto() // rule result
    debugger.stepInto() // query union
    debugger.stepOver() // over edge call
    debugger.stepInto() // rule merge
    debugger.stepOver() // path call
    assertAtomResult(debugger, edgeTable(Seq("temp", "to"), 2 -> 3, 2 -> 6, 2 -> 7, 4 -> 6, 4 -> 7))
    stepTillFinish(debugger)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "query", args)
  }

  test("step over rec pattern 2 levels deep (part of scc)") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, sevenEdgePattern), emptyDataModel)
    val args = ValueTable.unit()
    debugger.entry("query", args)
    debugger.stepInto() // computed
    debugger.stepInto() // rule merge
    debugger.stepInto() // path call
    debugger.stepOver() // over edge call
    debugger.stepInto() // rule merge
    debugger.stepInto() // rule result
    debugger.stepInto() // query union
    debugger.stepOver() // over edge call
    debugger.stepInto() // rule merge
    debugger.stepInto() // path call
    debugger.stepOver() // over edge call
    debugger.stepInto() // rule merge
    debugger.stepInto() // rule result
    debugger.stepInto() // query union
    debugger.stepOver() // over edge call
    debugger.stepInto() // rule merge
    debugger.stepOver() // path call
    assertAtomResult(debugger, edgeTable(Seq("temp", "to"), 6 -> 7))
    stepTillFinish(debugger)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "query", args)
  }

  test("step over rec pattern 2 levels deep (part of scc) with cyclic data") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, cycleEdgePattern), emptyDataModel)
    val args = ValueTable.unit()
    debugger.entry("query", args)
    debugger.stepInto() // computed
    debugger.stepInto() // rule merge
    debugger.stepInto() // path call
    debugger.stepOver() // over edge call
    debugger.stepInto() // rule merge
    debugger.stepInto() // rule result
    debugger.stepInto() // query union
    debugger.stepOver() // over edge call
    debugger.stepInto() // rule merge
    debugger.stepInto() // path call
    debugger.stepOver() // over edge call
    debugger.stepInto() // rule merge
    debugger.stepInto() // rule result
    debugger.stepInto() // query union
    debugger.stepOver() // over edge call
    debugger.stepInto() // rule merge
    debugger.stepOver() // path call
    assertAtomResult(debugger, edgeTable(Seq("temp", "to"), 6 -> 7, 3 -> 1))
    stepTillFinish(debugger)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "query", args)
  }

  test("example paper") {
    val debugger = initDebugger(
      module(pathPattern, edgePattern(1 -> 2, 2 -> 3, 3 -> 4, 3 -> 1)),
      emptyDataModel,
      emptyInput)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step out rec pattern (depth 1)") {
    val input = constructInput(Seq(1 -> 2, 2 -> 3, 3 -> 1))
    val debugger = initDebugger(module(pathPatternExt), new DataModel(), input)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    debugger.stepOut()
    assertQueryResult(
      debugger,
      "path",
      edgeTable(Seq("from", "to"), 1 -> 1, 1 -> 2, 1 -> 3)
    )
    stepTillFinish(debugger)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step out rec pattern (depth 2)") {
    val debugger =
      initDebugger(module(pathPattern, cycleEdgePattern), emptyDataModel)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    debugger.stepOver() // over edge call
    debugger.stepInto() // rule merge
    debugger.stepInto() // rule result
    debugger.stepInto() // query union
    debugger.stepOver() // over edge call
    debugger.stepInto() // rule merge
    debugger.stepInto() // path call
    debugger.stepOut()
    assertQueryResult(
      debugger,
      "path",
      edgeTable(Seq("from", "to"), 2 -> 1, 2 -> 3, 2 -> 6, 4 -> 6, 2 -> 7, 4 -> 7)
    )
    stepTillFinish(debugger)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("unbalanced transitive path") {
    val input = constructInput(Seq(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5))
    val debugger = initDebugger(module(pathPatternExt), new DataModel(), input)
    val args = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1)), Seq(ScalaValue(3))))
    debugger.entry("path", args)
    while (!debugger.isFinished)
      debugger.stepInto()
    assert(debugger.isFinished)

    assertExpectedTable(debugger, "path", args)
  }
}
