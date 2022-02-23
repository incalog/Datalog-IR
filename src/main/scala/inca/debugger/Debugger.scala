package inca.debugger

import inca.backend.analyze.DependencyGraph
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.{CountAggregation, CustomAggregation}
import inca.compiler.CompiledModule
import inca.debugger.table.Table
import inca.runtime.context.QueryScope
import inca.runtime.db.Database
import inca.runtime.index.dynamic.ParentIndex
import inca.runtime.index.virtual.{NodeNotLinkedIndex, NotNodeTypeIndex, SizeIndex}
import inca.runtime.index._
import inca.runtime.{EnginePool, Query}
import inca.util.{Derivative, Gensym, Scala}
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
import org.eclipse.viatra.query.runtime.matchers.tuple.{TupleMask, Tuples}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truechange.{EditScript, URI}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

trait Debugger extends DebuggerAPI {
  // Datalog program information
  private var compiled: CompiledModule = _
  protected lazy val dependencyGraph = new DependencyGraph(compiled.ir)

  // Extensional database stuff
  protected var database: Database = _
  private var engine: AdvancedViatraQueryEngine =  _
  protected var tableOps: TableOps = _

  // Debugger state
  private var fixpointState: FixpointState[Value] = _
  protected val callStack: CallStack = new CallStack()
  private val _controlTrace: ListBuffer[ControlPoint] = ListBuffer.empty
  protected val _breakpoints: mutable.Set[ControlPoint] = mutable.Set()

  // we store the derived tuples for a given pattern before executing the pattern to check if we reached a fixpoint
  private var lastDerivedTuples: Table[Value] = _



  // Accessor methods of debugger state

  val frameDeriv: Derivative[CallStack, Frame] = callStack.addDerivative(_ => null.asInstanceOf[Frame]){ stack =>
    if (stack.isEmpty) null.asInstanceOf[Frame]
    else stack.top
  }
  @inline
  def frame: Frame = frameDeriv.value

  def currentPoint: ControlPoint = frame.cp
  def currentPattern: Datalog.Pattern = frame.cp.point.pat
  def currentAtom: Option[Datalog.Atom] = frame.cp.point.atom

  def varsIR: Table[Value] = frame.cp.point.bodies match {
    case BeforeList => frame.argsTable
    case AtListElem(_, _, _) => frame.bodyTable
    case AfterList =>
      fixpointState.relation(currentPattern.name, frame.argsTable)
  }

  def controlTraceIR: Seq[ControlPoint] = _controlTrace.toSeq

  protected def traceCurrentControlPoint(): Unit =
    _controlTrace += currentPoint

  def relation(name: String): Table[Value] = fixpointState.relation(name)
  def relation(name: String, args: Table[Value]): Table[Value] =
    fixpointState.relation(name, args)
  def isFinished: Boolean = callStack.isFinished

  // initialization methods
  def initialize(mod: CompiledModule): Unit = {
    compiled = mod
    val scope = new QueryScope(compiled.dataModel)
    val (_engine, _database) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    engine = _engine
    database = _database
    fixpointState = new FixpointState[Value](compiled.ir.patternMap)
    tableOps = new TableOps(database, compiled, fixpointState)
  }

  def updateExtensionalData(edits: EditScript): Unit =
    engine.delayUpdatePropagation { () =>
      database.processEditScript(edits)
    }

  // Debugger methods
  def entry(name: Datalog.Name, bindings: Table[Value]): Unit = {
    val pat = compiled.ir.patternMap(name)
    val cp = ControlPoint.patternEntryPoint(pat)
    val frame = Frame(cp, bindings, Table.empty)
    callStack.push(frame)
    traceCurrentControlPoint()
  }

  protected def stepIntoIR(): Unit = {
    currentAtom match {
      case Some(atom) =>
        stepIntoIRNextAtom(frame, atom)
      case None =>
        stepIntoIRPatternBoundary(frame)
    }
    if (callStack.nonEmpty)
      traceCurrentControlPoint()
  }

  protected def abortIfBodyFailed(): Unit =
    if (callStack.nonEmpty) {
      if (frame.bodyTable.isEmpty) {
        val pattern = frame.cp.point.pat
        // the empty body table has to range over all pattern parameters
        val emptyBodyTable = Table.empty[Value](pattern.params.map(_.name))
        callStack.update(frame.copy(cp = frame.cp.abortBody, bodyTable = emptyBodyTable))
      }
    }

  protected def stepIntoIRNextAtom(frame: Frame, atom: Datalog.Atom): Unit = {
    if (atom.asCall.isDefined) {
      stepIntoIRCall(frame, atom)
    } else {
      val nextBodyTable = tableOps.transitionAtomTables(frame, atom)
      val next = frame.cp.stepIntra.get // yields next atom
      callStack.update(frame.copy(cp = next, bodyTable = nextBodyTable))
    }
    abortIfBodyFailed()
  }

  protected def stepIntoIRCall(frame: Frame, atom: Datalog.Atom): Unit = atom match {
    case Datalog.Call(name, args, _, neg) =>
      val pattern = compiled.ir.patternMap(name)
      val argsTable = tableOps.prepareArgTableOfCall(frame, pattern, args)
      // if we called the pattern already before with the same argument lookup table
      // else step into pattern
      fixpointState.addQuery(name, argsTable) match {
        case Some(query) =>
          val callee = ControlPoint(PatternPoint(pattern, BeforeList))
          if (neg) {
            checkNegativeCallArguments(args, frame.bodyTable)
          }
          callStack.push(Frame(callee, query, query))
        case None =>
          val patternTable = tableOps.transitionReturnCallTables(frame, name, argsTable, neg)
          val next = frame.cp.stepOver.get
          callStack.update(Frame(next, patternTable))
      }
    case Datalog.Computed(lhs, agg: Datalog.CountAggregation) =>
      val pattern = compiled.ir.patternMap(agg.patName)
      val argTable = tableOps.prepareArgTableOfCall(frame, pattern, agg.args)
      fixpointState.addQuery(agg.patName, argTable) match {
        case Some(query) =>
          val callee = ControlPoint(PatternPoint(pattern, BeforeList))
          callStack.push(Frame(callee, query, query))
        case None =>
          val patternTable = fixpointState.relation(agg.patName, argTable)
          val next = frame.cp.stepIntra.getOrElse(throw IllegalDebugStateException("Cannot have non-atom frame below pattern end frame on call stack"))
          val tables = tableOps.transitionCountAggTables(frame, patternTable, lhs)
          callStack.update(Frame(next, tables))
      }
    case Datalog.Computed(lhs, agg: Datalog.CustomAggregation) =>
      val pattern = compiled.ir.patternMap(agg.patName)
      val argTable = tableOps. prepareArgTableOfCall(frame, pattern, agg.args)
      fixpointState.addQuery(agg.patName, argTable) match {
        case Some(query) =>
          val callee = ControlPoint(PatternPoint(pattern, BeforeList))
          callStack.push(Frame(callee, query, query))
        case None =>
          val patternTable = fixpointState.relation(agg.patName, argTable)
          val next = frame.cp.stepIntra.getOrElse(throw IllegalDebugStateException("Cannot have non-atom frame below pattern end frame on call stack"))
          val tables = tableOps.transitionCustomAggTables(frame, patternTable, lhs, agg)
          callStack.update(Frame(next, tables))
      }
    case _ => throw IllegalDebugStateException(s"Atom $atom should contain call")
  }

  private def checkNegativeCallArguments(args: Seq[Datalog.Term], table: Table[Value]): Unit = {
    args.foreach {
      case Datalog.Var(name) if !table.isBound(name) =>
        throw IllegalDebugStateException(s"All arguments of a negative pattern call have to be bound, but $name is not bound")
      case _ => // do nothing
    }
  }

  protected def doPatternEntry(cp: ControlPoint): Unit = {
    val next = cp.stepIntra.get // yields first body of this pattern
    lastDerivedTuples = fixpointState.relation(cp.point.pat.name, frame.argsTable)
    callStack.update(Frame(next, frame.argsTable, frame.argsTable))
  }

  protected def doPatternExit(frame: Frame): Unit = {
    val pat = frame.cp.point.pat
    callStack.pop() // pop pattern exit point

    // if the fixpoint of the call has not been reached call pattern again
    val currentTable = fixpointState.relation(pat.name, frame.argsTable)
    val fullTable = readDatabase(pat.name, frame.argsTable)
    val notEqToBottomUpTable = currentTable != fullTable
    val newTupledDerived = !currentTable.diff(lastDerivedTuples).isEmpty
    if (notEqToBottomUpTable && newTupledDerived) {
      val nextFrame = Frame(ControlPoint.patternEntryPoint(pat), frame.argsTable, frame.argsTable)
      callStack.push(nextFrame)
      return
    }

    // if the stack is still not empty there should be a call, or an aggregation on top
    if (callStack.nonEmpty) {
      val callerFrame = callStack.top
      val next = callerFrame.cp.stepIntra
        .getOrElse(throw IllegalDebugStateException("Cannot have non-atom frame below pattern end frame on call stack"))
      val tables = callerFrame.cp.atom match {
        case Datalog.Call(_, _, _, false) =>
          tableOps.transitionReturnCallTables(callerFrame, frame)
        case Datalog.Call(_, _, _, true) =>
          tableOps.transitionReturnNegCallTables(callerFrame, frame)
        case Datalog.Computed(lhs, custAgg: CustomAggregation) =>
          val patternTable = fixpointState.relation(custAgg.patName, frame.argsTable)
          tableOps.transitionCustomAggTables(callerFrame, patternTable, lhs, custAgg)
        case Datalog.Computed(lhs, countAgg: CountAggregation) =>
          val patternTable = fixpointState.relation(countAgg.patName, frame.argsTable)
          tableOps.transitionCountAggTables(callerFrame, patternTable, lhs)
        case atom => throw new MatchError(atom, "should be a call or an aggregation")
      }
      callStack.update(Frame(next, tables))
    }
  }

  protected def doBodyEntry(frame: Frame, cp: ControlPoint): Unit = {
    val next = cp.stepIntra.get // yields first atom of this body
    callStack.update(Frame(next, frame.argsTable, frame.argsTable))
  }

  protected def doBodyExit(frame: Frame, cp: ControlPoint): Unit = {
    val next = cp.stepIntra.get // yields entry of next body
    val pattern = frame.cp.point.pat
    val params = pattern.params.map(_.name)
    val projectedBodyTable = frame.bodyTable.project(params)
    fixpointState.addDerivedTuples(pattern.name, projectedBodyTable)
    callStack.update(Frame(next, frame.argsTable, frame.argsTable))
  }

  private def stepIntoIRPatternBoundary(frame: Frame): Unit = {
    val cp = frame.cp
    if (cp.point.isPatternEntry) {
      doPatternEntry(cp)
    } else if (cp.point.isPatternExit) {
      doPatternExit(frame)
    } else if (cp.point.isBodyEntry) {
      doBodyEntry(frame, cp)
    } else if (cp.point.isBodyExit) {
      doBodyExit(frame, cp)
    } else {
      throw new IllegalStateException(s"Unexpected control point $cp")
    }
  }




  protected def stepOverIR(): Unit = {
    val frame0 = callStack.top
    frame0.cp.point.atom match {
      case Some(atom) =>
        if (atom.asCall.isDefined)
          stepOverCall(frame, atom)
        else
          stepIntoIRNextAtom(frame, atom)
      case None =>
        if (frame0.cp.isPatternPoint) {
          val pattern = frame0.cp.point.pat
          lastDerivedTuples = fixpointState.relation(pattern.name, frame0.argsTable)
          val patternTable = readDatabase(pattern.name, frame0.argsTable)
          fixpointState.addDerivedTuples(pattern.name, patternTable)
          val next = ControlPoint(PatternPoint(pattern, AfterList))
          callStack.update(Frame(next, frame0.argsTable, patternTable))
        } else if (frame0.cp.isBodyPoint) {
          // we cannot read from the database because we dont know which tuples where derived by a specific body
          // we step into until the next breakpoint is reached where the stack size does not change
          val next = frame0.cp.stepOver.get
          runUntil(next)
        } else {
          stepIntoIRPatternBoundary(frame0)
        }
    }
    if (callStack.nonEmpty)
      traceCurrentControlPoint()
  }

  private def stepOverCall(frame: Frame, atom: Datalog.Atom): Unit = {
    val (name, args) = atom.asCall.get
    val calledPat = compiled.ir.patternMap(name)
    val argsTable = tableOps.prepareArgTableOfCall(frame, calledPat, args)
    val calledPatTable =
      if (isPartOfCurrentSCC(atom)) {
        def stepIntoUntil(stop: () => Boolean): Unit =
          while (!stop())
            stepInto()

        // compute fixpoint of current call and top is
        val patternEntryPoint = ControlPoint.patternEntryPoint(calledPat)
        val controlPointTarget = patternEntryPoint.stepOver.getOrElse(throw IllegalDebugStateException("Pattern entry point has to have a corresponding pattern exit"))
        val currentStackSize = callStack.size
        var currentTable = fixpointState.relation(name, argsTable)

        // compute fixpoint of current call
        stepIntoUntil(() => {
          val newTable = fixpointState.relation(name, argsTable)
          val fixpointReached = currentTable == newTable
          currentTable = newTable
          fixpointReached && currentStackSize == callStack.size && controlPointTarget == callStack.top.cp
        })
        currentTable
      } else {
        // we can read because pattern belongs to lower scc
        val table = readDatabase(name, argsTable)
        // we extend the fixpoint state accordingly
        fixpointState.addQuery(name, argsTable)
        fixpointState.addDerivedTuples(name, table)
        table
      }
    atom match {
      case Datalog.Call(_, args, _, neg) =>
        val params = calledPat.params.map(_.name)
        val tables =
          if (neg) {
            checkNegativeCallArguments(args, frame.bodyTable)
            tableOps.transitionReturnNegCallTables(frame, params, calledPatTable)
          } else {
            tableOps.transitionReturnCallTables(frame, params, calledPatTable)
          }
        val next = frame.cp.stepOver.get
        callStack.update(Frame(next, tables))
      case Datalog.Computed(lhs, _: CountAggregation) =>
        val tables = tableOps.transitionCountAggTables(frame, calledPatTable, lhs)
        val next = frame.cp.stepOver.get
        callStack.update(Frame(next, tables))
      case Datalog.Computed(lhs, agg: CustomAggregation) =>
        val tables = tableOps.transitionCustomAggTables(frame, calledPatTable, lhs, agg)
        val next = frame.cp.stepOver.get
        callStack.update(Frame(next, tables))
      case _ => throw new MatchError(s"Not a call: $atom")
    }
  }

  private def isPartOfCurrentSCC(atom: Datalog.Atom): Boolean = {
    val (name, _) = atom.asCall.get
    dependencyGraph.inSameStronglyConnectedCompontent(name, callStack.top.cp.point.pat.name)
  }

  private def runUntil(cp: ControlPoint): Unit = {
    val currentStackSize = callStack.size
    // we run stepInto until we reach the target controlpoint and the stack size is the same as it was before
    while (!(frame.cp == cp && callStack.size == currentStackSize)) {
      stepIntoIR()
    }
  }


  protected def stepOutIR(): Unit = {
    val frame0 = callStack.top
    frame0.cp.stepOut match {
      case Some(next) =>
        val currentStackSize = callStack.size
        while (!(currentStackSize == callStack.size && callStack.top.cp == next)) {
          stepOverIR()
        }
      case None =>
        // we are at an pattern exit
        doPatternExit(frame0)
    }
  }

  def readDatabase(name: String, bindings: Table[Value]): Table[Value] = {
    val mainSpec = compiled.psystemModule.patterns.get(name) match {
      case Some(spec) => spec()
      case None => return Table.empty
    }
    val mainMatcher = engine.getMatcher(mainSpec)
    val unboundCols = compiled.ir.patternMap(name).params.map(_.name).diff(bindings.columns)
    val rows = bindings.rows.flatMap { row =>
      val inputMap = bindings.columns.zip(row.map(_.unwrap)).toMap ++ unboundCols.map( _ -> null)
      val input = Query.Match(mainSpec, inputMap, isMutable = false)
      val matches = mainMatcher.getAllMatches(input)
      matches.asScala.map { m =>
        m.toArray.map {
          case uri: URI => URIValue(uri)
          case v: Any => ScalaValue(v)
        }.toSeq
      }
    }.toSeq
    Table(mainMatcher.getParameterNames.asScala.toSeq, rows)
  }


  /** Breakpoint related functionality */

  override def clearBreakpoints(): Unit = _breakpoints.clear()

  override def resume(): Unit = ???

}