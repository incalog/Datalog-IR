package inca.debugger

import inca.backend.ir.Datalog
import inca.backend.optimize.EliminateNonproductiveRelations
import org.scalatest.funsuite.AnyFunSuite
import inca.compiler.{CompiledModule, Compiler, Options}
import inca.debugger.table.ImmutableTable
import inca.runtime.EnginePool
import inca.runtime.context.{DataModel, QueryScope}
import inca.runtime.db.DatabaseInput
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truechange.EditScript
import inca.debugger.Value.{topAndBotFactory, valueOrdering}
import inca.debugger.old.{ControlPoint, Debugger, IRDebugger}
import inca.examples.functional.LambdaCalculus
import inca.examples.functional.LambdaCalculus.{app, lam, num, tint, vari}
import inca.frontend.functional.compiler.FunctionalOptions
import inca.frontend.functional.debugger.FunctionalDebugger

import scala.meta.Term
import scala.meta.quasiquotes._

class CountPrimitiveSteps extends AnyFunSuite {

  def initDebugger(module: CompiledModule, dataModel: DataModel, dbInput: DatabaseInput): IRDebugger = {
    val debugger = new IRDebugger(module)
    initDatabaseRuntime(debugger, dataModel, dbInput)
    debugger
  }

  def initDatabaseRuntime(debugger: IRDebugger, dataModel: DataModel, dbInput: DatabaseInput): Unit = {
    val scope = new QueryScope(dataModel)
    val (_engine, _database) =
      EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    _engine.delayUpdatePropagation(() => {
      _database.processDatabaseInput(dbInput)
    })
    debugger.setDatabaseRuntime(_engine, _database)
  }

  def stepNumOfTimes(debugger: Debugger, steps: Int): Unit =
    (0 until steps).foreach { _ =>
      if(!debugger.isFinished)
        debugger.stepInto()
    }

  def module(pats: Datalog.Pattern*): Datalog.Module = Datalog.Module("Module", Seq(), pats, Seq())

  val pathPattern: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "path",
      Seq(Datalog.Param("from", Datalog.base.TScalaInt), Datalog.Param("to", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(
          Seq(
            Datalog.ExtensionalCall("edge", Seq(Datalog.Var("from"), Datalog.Var("to")))
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.ExtensionalCall("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
            Datalog.Call("path", Seq(Datalog.Var("temp"), Datalog.Var("to")))
          )
        )
      )
    )

  def edgesToDBInput(edges: Seq[(Int, Int)]): DatabaseInput = {
    val insertions = edges.map { case (from, to) =>
      Tuples.staticArityFlatTupleOf(from, to)
    }.toSet
    DatabaseInput(EditScript(Seq()), Map("edge" -> insertions), Map())
  }

  // key is number of step intos beforehand
  // value (current control point, visited control points of stepInto, stepIntoSteps, stepOverSteps)
  def recordStats(debuggerFactory: Seq[(Int, Int)] => IRDebugger, edges: Seq[(Int, Int)]): Map[Int, (ControlPoint, Seq[ControlPoint], Int, Int)] = {
    val maxStepCountDebugger = debuggerFactory(edges)
    while(!maxStepCountDebugger.isFinished)
      maxStepCountDebugger.stepInto()
    val maxNumberOfSteps = maxStepCountDebugger.controlTraceIR.size


    println(maxNumberOfSteps)
    (0 until maxNumberOfSteps).flatMap { startingStep =>
      val stepOverDebugger = debuggerFactory(edges)
      val stepIntoDebugger = debuggerFactory(edges)
      stepNumOfTimes(stepOverDebugger, startingStep)
      stepNumOfTimes(stepIntoDebugger, startingStep)
      val currentCP = stepIntoDebugger.currentPoint
      stepIntoDebugger.currentPoint.stepOver match {
        case Some(trgCP) =>
          val ctSizeBefore = stepIntoDebugger.controlTraceIR.size
          val bp = stepIntoDebugger.currentFrameBreakpoint(trgCP)
          stepIntoDebugger.addBreakpoint(bp)
          stepIntoDebugger.resumeWithStepInto()
          val stepIntoSteps = stepIntoDebugger.controlTraceIR.size - ctSizeBefore

          val soctSizeBefore = stepOverDebugger.controlTraceIR.size
          stepOverDebugger.stepOver()
          val stepOverSteps = stepOverDebugger.controlTraceIR.size - soctSizeBefore

          val stepIntoTrace = stepIntoDebugger.controlTraceIR.takeRight(stepIntoSteps)
          Some(startingStep -> (currentCP, stepIntoTrace, stepIntoSteps, stepOverSteps))
        case None => None
      }
    }.toMap
  }

  def initPathDebugger(edges: Seq[(Int, Int)]): IRDebugger = {
    val compiled = Compiler.compileGP(module(pathPattern), new DataModel(), Options())
    val debugger = initDebugger(compiled, new DataModel(), edgesToDBInput(edges))
    val entryTable = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", entryTable)
    debugger
  }

  def chain(from: Int, to: Int): Seq[(Int, Int)] =
    (from until to).map { i =>
      (i, i + 1)
    }


  test("print steps") {
    // val edges = Seq(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 3 -> 5, 5 -> 2)
    val edges = chain(1, 20) ++ Seq((10, 4), (8, 5), (7, 6))
    val stats = recordStats(initPathDebugger, edges)
    val recCallStats = stats.filter { case (i, (cp, _, _, _)) =>
      cp.isAtomPoint && (cp.atom match {
        case _: Datalog.Call => true
        case _ => false
      })
    }

    val moreSiSteps = stats.filter { case (i, (_, _, si, so)) => si > so }

    moreSiSteps.toSeq.sortBy(_._1).foreach { case (idx, (cp, _, si, so)) =>
      println((idx, cp, si, so))
    }
    val statsRatio: Iterable[Double] = stats.values.map { case (_, _, si, so) =>
      if (so == 0) 0.0
      else si.toDouble / so.toDouble
    }
  }

  // functional debugging steps
  val tcProg: String = LambdaCalculus.typeOfModule
  val inputProg: Term = app(lam("x", tint, vari("x")), num(12))

  def initTCDebugger: FunctionalDebugger = {
    val compiled = Compiler.compileFunctional(tcProg, FunctionalOptions().withOptimizations(Seq(EliminateNonproductiveRelations)))
    val debugger = new FunctionalDebugger(compiled)
    setupDatabaseRuntime(debugger, compiled.dataModel)
    debugger
  }

  def setupDatabaseRuntime(
      debugger: FunctionalDebugger,
      dataModel: DataModel,
      es: EditScript = EditScript(Seq())): Unit = {
    val scope = new QueryScope(dataModel)
    val (_engine, _database) =
      EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    _engine.delayUpdatePropagation(() => {
      _database.processEditScript(es)
    })
    debugger.setDatabaseRuntime(_engine, _database)
  }

  // TODO FIX
  test("functional debugger") {
    val debugger = initTCDebugger
    debugger.entry("main", inputProg)
    println(inputProg)
    while(!debugger.isFinished) {
      println(debugger.currentDebuggerInfo())
      debugger.stepInto()
    }
    val steps = debugger.controlTraceFrontend.size
  }

  val adaptedPathPattern: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "path",
      Seq(Datalog.Param("from", Datalog.base.TScalaInt), Datalog.Param("to", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(
          Seq(
            Datalog.ExtensionalCall("edge", Seq(Datalog.Var("from"), Datalog.Var("to")))
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.ExtensionalCall("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
            Datalog.Compare(Datalog.EqComparator, Datalog.Var("temp"), Datalog.Constant(Datalog.base.IntLiteral(2))),
            Datalog.Call("path", Seq(Datalog.Var("temp"), Datalog.Var("to")))
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.ExtensionalCall("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
            Datalog.Compare(Datalog.EqComparator, Datalog.Var("temp"), Datalog.Constant(Datalog.base.IntLiteral(3))),
            Datalog.Call("path", Seq(Datalog.Var("temp"), Datalog.Var("to")))
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.ExtensionalCall("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
            Datalog.Call("path", Seq(Datalog.Var("temp"), Datalog.Var("to")))
          )
        )
      )
    )

  test("usecase path 1") {
    val edges = Seq(1 -> 2, 2 -> 1, 1 -> 3)
    val debugger = initPathDebugger(edges)
    val entryTable = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", entryTable)
    // stepNumOfTimes(debugger, 4)
    println(debugger.currentPoint)
    println(debugger.relation("path"))
    debugger.stepInto()
    // before first body
    println(debugger.currentPoint)
    println(debugger.relation("path"))
    debugger.stepOver()
    // after first body
    println(debugger.currentPoint)
    println(debugger.relation("path"))
    debugger.stepInto()
    // before second body
    println(debugger.currentPoint)
    println(debugger.relation("path"))
    debugger.stepOver()
    println(debugger.currentPoint)
    println(debugger.relation("path"))
    debugger.stepInto()
    println(debugger.currentPoint)
    println(debugger.relation("path"))
  }

}
