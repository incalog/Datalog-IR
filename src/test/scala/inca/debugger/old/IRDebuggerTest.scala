package inca.debugger.old

import inca.analyzedLangs.Exp
import inca.analyzedLangs.ExpLangTestAnalyses
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.base.TScalaInt
import inca.compiler.CompiledDatalogModule
import inca.compiler.Options
import inca.debugger.table.ImmutableTable
import inca.debugger.ExamplePrograms._
import inca.debugger.ScalaValue
import inca.debugger.URIValue
import inca.debugger.Value
import inca.runtime.context.DataModel
import inca.runtime.context.QueryScope
import inca.runtime.db.DatabaseInput
import inca.runtime.EnginePool
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Assertion
import org.scalatest.BeforeAndAfterEach
import truechange.EditScript

class IRDebuggerTest extends AnyFunSuite with BeforeAndAfterEach {

  override def afterEach(): Unit = {
    EnginePool.disposeAllEngines()
  }

  val emptyDataModel = new DataModel()

  def stepTillFinish(debugger: IRDebugger): Unit = {
    while (!debugger.isFinished) {
      println(debugger.frame)
      if (debugger.frame.cp.isAtomPoint) {
        println(debugger.frame.cp.atom)
      }
      debugger.stepInto()
    }
  }

  def initDatabaseRuntime(debugger: Debugger, dataModel: DataModel, es: EditScript): Unit = {
    val scope = new QueryScope(dataModel)
    val (_engine, _database) =
      EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    _engine.delayUpdatePropagation(() => {
      _database.processEditScript(es)
    })
    debugger.setDatabaseRuntime(_engine, _database)
  }

  def initDebugger(
      module: Datalog.Module,
      dataModel: DataModel,
      es: EditScript = EditScript(Seq())
    ): IRDebugger = {
    val compiled = CompiledDatalogModule(module, dataModel, Options())
    val debugger = new IRDebugger(compiled)
    initDatabaseRuntime(debugger, dataModel, es)
    debugger
  }

  def assertExpectedTable(
      debugger: IRDebugger,
      name: String,
      args: ImmutableTable[Value]
    ): Assertion = {
    val derived = debugger.relation(name, args)
    val expected = debugger.readDatabase(name, args)
    assertResult(expected)(derived)
  }

  // Step into tests
  test("simple stepinto of single call") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("one", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "one", args)
  }

  test("simple stepinto") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "two", args)
  }

  test("test compare atoms") {
    val debugger = initDebugger(module(comparatorPattern), emptyDataModel)
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

//  test("test count aggregation 1") {
//    val debugger = initDebugger(module(countAggPattern, sevenEdgePattern), emptyDataModel)
//    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
//    debugger.entry("numberOfEdges", args)
//    stepTillFinish(debugger)
//
//    assertExpectedTable(debugger, "numberOfEdges", args)
//  }
//
//  test("test count aggregation 2") {
//    val debugger = initDebugger(module(countAggPattern, sevenEdgePattern), emptyDataModel)
//    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(7))))
//    debugger.entry("numberOfEdges", args)
//    stepTillFinish(debugger)
//
//    assertExpectedTable(debugger, "numberOfEdges", args)
//  }
//
//  test("test count aggregation 3") {
//    val debugger = initDebugger(module(countAggPatternCompareWithConst, sevenEdgePattern), emptyDataModel)
//    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
//    debugger.entry("numberOfEdges", args)
//    stepTillFinish(debugger)
//
//    assertExpectedTable(debugger, "numberOfEdges", args)
//  }
//
//  test("test count aggregation 4") {
//    val debugger = initDebugger(module(countAggPatternCompareWithConst, sevenEdgePattern), emptyDataModel)
//    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(2))))
//    debugger.entry("numberOfEdges", args)
//    stepTillFinish(debugger)
//
//    assertExpectedTable(debugger, "numberOfEdges", args)
//  }
//
//  test("custom aggregation example") {
//    val compiledExample = Compiler.compileFunctional(Code.simpleFoldIntModule, FunctionalOptions())
//
//    val debugger = initDebugger(compiledExample.ir, new DataModel())
//    val args = Table[Value](Seq("start", "end"), Seq(Seq(ScalaValue(0), ScalaValue(4))))
//    debugger.entry("sum", args)
//    stepTillFinish(debugger)
//
//    assertExpectedTable(debugger, "sum", args)
//  }

  // recursive step into
  test("step into recursive pattern") {
    val debugger = initDebugger(module(sevenEdgePattern, pathPattern), emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
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

  // step over tests
  test("step over pattern") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    debugger.stepOver()
    debugger.stepOver()

    assertExpectedTable(debugger, "two", args)
  }

  test("step over body") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    debugger.stepInto()
    debugger.stepOver() // step over body

    debugger.stepOver() // needed to step to pattern exit
    debugger.stepOver() // needed to exit pattern

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "two", args)
  }

  test("step over non-rec call") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    val args = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    debugger.stepInto()
    debugger.stepInto() // step into body
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
    debugger.stepInto() // step into body
    debugger.stepOver() // step over computed
    debugger.stepOver() // step over negated path call

    debugger.stepOver() // needed to step to pattern exit
    debugger.stepOver() // needed to exit pattern

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "query", args)
  }

  test("step over rec pattern (not part of scc) with cyclic data") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, cycleEdgePattern), emptyDataModel)
    val args = ImmutableTable.unit[Value]()
    debugger.entry("query", args)
    debugger.stepInto() // step into pattern
    debugger.stepInto() // step into body
    debugger.stepOver() // step over computed
    debugger.stepOver() // step over negated path call

    debugger.stepOver() // needed to step to pattern exit
    debugger.stepOver() // needed to exit pattern

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "query", args)
  }

  test("step over rec pattern (part of scc)") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, sevenEdgePattern), emptyDataModel)
    val args = ImmutableTable.unit[Value]()
    debugger.entry("query", args)
    assert(debugger.frame.cp.isPatternEntry)
    debugger.stepInto() // step into pattern
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepInto() // step into body
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOver() // step over computed
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepInto() // step into path call
    assert(debugger.frame.cp.isPatternEntry)
    debugger.stepInto() // step into pattern
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepOver() // step over first body
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepInto() // step into second body
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepInto() // step to edge call
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOver() // step over edge call
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOver() // step over recursive path call

    val currentDerived =
      debugger.relation(
        "path",
        ImmutableTable[Value](
          Seq("from"),
          Seq(Seq(ScalaValue(4)), Seq(ScalaValue(2)), Seq(ScalaValue(5))))
      )
    val expected = edgeTable(Seq("from", "to"), 4 -> 6, 2 -> 3, 2 -> 6, 4 -> 7, 2 -> 7)
    assertResult(expected)(currentDerived)
    assert(debugger.frame.cp.isBodyExit)

    debugger.stepOver() // needed to step to pattern exit (of path call)
    assert(debugger.frame.cp.isPatternExit)
    debugger.stepOver() // needed to step to body exit
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepOver() // needed to step to pattern exit (of notTargetof)
    assert(debugger.frame.cp.isPatternExit)
    debugger.stepOver() // pop pattern exit (of notTargetof)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "query", args)
  }

  test("step over rec pattern 2 levels deep (part of scc)") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, sevenEdgePattern), emptyDataModel)
    val args = ImmutableTable.unit[Value]()
    debugger.entry("query", args)
    assert(debugger.frame.cp.isPatternEntry)
    debugger.stepInto() // step into pattern
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepInto() // step into body
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOver() // step over computed
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepInto() // step into negated path call
    assert(debugger.frame.cp.isPatternEntry)
    debugger.stepInto() // step into pattern
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepOver() // step over first body
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepInto() // step into second body
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepInto() // step to edge call
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOver() // step over edge call
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepInto() // step into recursive path call
    assert(debugger.frame.cp.isPatternEntry)
    debugger.stepInto() // step into pattern
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepOver() // step over first body
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepInto() // step into second body
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepInto() // step to edge call
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOver() // step over edge call
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOver() // step over recursive path call
    val currentDerived =
      debugger.relation(
        "path",
        ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(3)), Seq(ScalaValue(6))))
      )
    val expected = edgeTable(Seq("from", "to"), 6 -> 7)
    assertResult(expected)(currentDerived)
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepOver() // needed to step to pattern exit (of path call)
    assert(debugger.frame.cp.isPatternExit)
    debugger.stepOver() // needed to step to pattern exit (of path call)
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepOver() // needed to step to pattern exit (of path call)
    assert(debugger.frame.cp.isPatternExit)
    debugger.stepOver() // needed to step to body exit
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepOver() // needed to step to pattern exit (of notTargetof)
    assert(debugger.frame.cp.isPatternExit)
    debugger.stepOver() // pop pattern exit (of notTargetof)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "query", args)
  }

  test("step over rec pattern 2 levels deep (part of scc) with cyclic data") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, cycleEdgePattern), emptyDataModel)
    val args = ImmutableTable.unit[Value]()
    debugger.entry("query", args)
    assert(debugger.frame.cp.isPatternEntry)
    debugger.stepInto() // step into pattern
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepInto() // step into body
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOver() // step over computed
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepInto() // step into negated path call
    assert(debugger.frame.cp.isPatternEntry)
    debugger.stepInto() // step into pattern
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepOver() // step over first body
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepInto() // step into second body
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepInto() // step to edge call
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOver() // step over edge call
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepInto() // step into recursive path call
    assert(debugger.frame.cp.isPatternEntry)
    debugger.stepInto() // step into pattern
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepOver() // step over first body
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepInto() // step into second body
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepInto() // step to edge call
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOver() // step over edge call
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOver() // step over recursive path call
    val currentDerived =
      debugger.relation(
        "path",
        ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(3)), Seq(ScalaValue(6))))
      )
    val expected =
      edgeTable(Seq("from", "to"), 6 -> 7, 3 -> 1, 3 -> 2, 3 -> 4, 3 -> 5, 3 -> 3, 3 -> 7, 3 -> 6)
    assertResult(expected)(currentDerived)
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepOver() // needed to step to pattern exit (of path call)
    assert(debugger.frame.cp.isPatternExit)
    debugger.stepOver() // needed to step to pattern exit (of path call)
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepOver() // needed to step to pattern exit (of path call)
    assert(debugger.frame.cp.isPatternExit)
    debugger.stepOver() // needed to step to body exit
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepOver() // needed to step to pattern exit (of notTargetof)
    assert(debugger.frame.cp.isPatternExit)
    debugger.stepOver() // pop pattern exit (of notTargetof)
    assert(debugger.isFinished)
    assertExpectedTable(debugger, "query", args)
  }

  test("step out of pattern") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, cycleEdgePattern), emptyDataModel)
    val args = ImmutableTable.unit[Value]()
    debugger.entry("query", args)
    assert(debugger.frame.cp.isPatternEntry)
    debugger.stepOut()
    assert(debugger.frame.cp.isPatternExit)
    assertExpectedTable(debugger, "query", args)
  }

  test("step out of body") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, cycleEdgePattern), emptyDataModel)
    val args = ImmutableTable.unit[Value]()
    debugger.entry("query", args)
    assert(debugger.frame.cp.isPatternEntry)
    debugger.stepInto()
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepInto()
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOut()
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepOver()
    assert(debugger.frame.cp.isPatternExit)
    assertExpectedTable(debugger, "query", args)
  }

  test("step out inner call") {
    val debugger =
      initDebugger(module(query, nodePattern, pathPattern, cycleEdgePattern), emptyDataModel)
    val args = ImmutableTable.unit[Value]()
    debugger.entry("query", args)
    assert(debugger.frame.cp.isPatternEntry)
    debugger.stepInto()
    assert(debugger.frame.cp.isBodyEntry)
    debugger.stepInto()
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepOver()
    assert(debugger.frame.cp.isAtomPoint)
    debugger.stepInto()
    assert(debugger.frame.cp.isPatternEntry)
    debugger.stepOut()
    assert(debugger.frame.cp.isPatternExit)
    debugger.stepOut()
    assert(debugger.frame.cp.isBodyExit)
    debugger.stepOut()
    assert(debugger.frame.cp.isPatternExit)
    assertExpectedTable(debugger, "query", args)
  }

  val extCallEdgePattern: Datalog.Pattern = Datalog.Pattern(
    None,
    "edge",
    Seq(
      Datalog.Param("x", TScalaInt),
      Datalog.Param("y", TScalaInt)
    ),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.ExtensionalCall("extEdge", Seq(Datalog.Var("x"), Datalog.Var("y")))
        ))
    )
  )
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
    val debugger = initDebugger(module(extCallEdgePattern), emptyDataModel)
    debugger.updateExtensionalData(extEdgeInput)
    val args = ImmutableTable.unit[Value]()
    debugger.entry("edge", args)
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "edge", args)
  }

  test("ext call with one argument bound") {
    val debugger = initDebugger(module(extCallEdgePattern), emptyDataModel)
    debugger.updateExtensionalData(extEdgeInput)
    val args = ImmutableTable[Value](Seq("x"), Seq(Seq(ScalaValue(1)), Seq(ScalaValue(2))))
    debugger.entry("edge", args)
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "edge", args)
  }

  test("ext call with both arguments bound") {
    val debugger = initDebugger(module(extCallEdgePattern), emptyDataModel)
    debugger.updateExtensionalData(extEdgeInput)
    val args = ImmutableTable[Value](Seq("y", "x"), Seq(Seq(ScalaValue(4), ScalaValue(3))))
    debugger.entry("edge", args)
    stepTillFinish(debugger)
    assertExpectedTable(debugger, "edge", args)
  }
}
