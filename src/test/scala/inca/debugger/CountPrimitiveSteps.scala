package inca.debugger

import inca.backend.ir.Datalog
import org.scalatest.funsuite.AnyFunSuite
import inca.compiler.{CompiledModule, Compiler, Options}
import inca.debugger.table.ImmutableTable
import inca.runtime.EnginePool
import inca.runtime.context.{DataModel, QueryScope}
import inca.runtime.db.DatabaseInput
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truechange.EditScript
import inca.debugger.Value.{valueOrdering, topAndBotFactory}

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
      Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
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

  test("print steps") {
    val edges = Seq(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 3 -> 5, 5 -> 2)
    val stats = recordStats(initPathDebugger, edges)
    val recCallStats = stats.filter { case (i, (cp, _, _, _)) =>
      cp.isAtomPoint && (cp.atom match {
        case _: Datalog.Call => true
        case _ => false
      })
    }

    val moreSiSteps = stats.filter { case (i, (_, _, si, so)) => si > so }
    moreSiSteps.foreach(println)
    val statsRatio: Iterable[Double] = stats.values.map { case (_, _, si, so) =>
      if (so == 0) 0.0
      else si.toDouble / so.toDouble
    }
  }
}
