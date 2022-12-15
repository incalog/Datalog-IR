package inca.debugger.redesign_new

import inca.backend.analyze.DependencyGraph
import inca.backend.ir.Datalog
import inca.backend.optimize.InlineSimpleRelations
import inca.compiler.CompiledDatalogModule
import inca.compiler.CompiledModule
import inca.debugger.table.indexing.IndexCover
import inca.debugger.table.ImmutableTable
import inca.debugger.table.IndexedTableFactory
import inca.debugger.DebuggerAPI
import inca.debugger.IllegalDebugStateException
import inca.debugger.Value
import inca.runtime.context.QueryScope
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import inca.runtime.EnginePool
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

/**
 */
trait Debugger extends DebuggerAPI {

  implicit private lazy val indexedTableFactory: IndexedTableFactory[Value] = {
    implicit val valueOrdering: Ordering[Value] = Value.valueOrdering
    val parametersOfRelations = predicates.map { case (name, pat) =>
      name -> pat.params.map(_.name)
    }
    new IndexedTableFactory[Value](parametersOfRelations)
  }

  // ****** global static information ******//
  private var module: CompiledDatalogModule = _
  private lazy val predicates: Map[Predicate, Datalog.Pattern] = module.ir.patternMap

  private[redesign_new] var state: DebuggerState = _
  private lazy val atomOps: AtomTableOps =
    new AtomTableOps(state.bottomUpRuntime, indexedTableFactory)
  protected[redesign_new] val queryStack: QueryStack = new QueryStack

  private lazy val dependencyGraph = new DependencyGraph(module.ir)
  protected lazy val breakpointHandler: BreakpointHandler =
    new BreakpointHandler(dependencyGraph)

  // This method initializes the debugger
  // It is required to call this method before using the debugger instance
  def initialize(module: CompiledModule): Unit = {
    if (module.options.optimizations.contains(InlineSimpleRelations))
      throw new IllegalArgumentException(s"Inlining must be deactivated for debugging.")
    this.module = CompiledDatalogModule(
      module.ir,
      module.dataModel,
      module.options.withTransformations(module.options.transformations :+ BlacklistTransformation))
  }

  def initializeDatabaseRuntime(input: DatabaseInput): Unit = {
    val scope = new QueryScope(module.dataModel)
    val (_engine, _database) =
      EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    _engine.delayUpdatePropagation(() => {
      _database.processDatabaseInput(input)
    })
    val rt = DatalogRuntime(_engine, _database, module)
    state = new DebuggerState(rt)
    // TODO we do this to initialize db before starting debugging session
    // state.countBottomUp("path", ImmutableTable.unit())
  }

  def entry(p: Predicate, args: ImmutableTable[Value]): Unit = {
    val params = predicates(p).params
    val rules = predicates(p).bodies
    val ruleEvals = rules.map { body =>
      Rule(p, params, body.atoms.map(Atom).toList)
    }.toList
    val next = Subquery(p, args, ImmutableTable.empty(params.map(_.name)), args, ruleEvals)
    state.addSeenQuery(p, args)
    queryStack.push(next)
    stepped()
  }

  final protected def doStepIntoIR(): Boolean = {
    val top = queryStack.top
    top match {
      case Subquery(_, _, _, _, Rule(_, _, Nil) :: _) =>
        ruleResult(top)
      case Subquery(_, _, _, _, Rule(_, _, AtomResult(_, _) :: _) :: _) =>
        ruleMerge(top)
      case Subquery(_, _, _, _, Rule(_, _, Atom(_) :: _) :: _) =>
        nextAtom(top)
      case Subquery(_, _, _, _, RuleResult(_, _) :: _) =>
        queryUnion(top)
      case Subquery(_, _, _, _, Nil) =>
        queryEnd(top)
    }
    // rule merge
    // rule result
    // a rules
    // q stable/iterate
    // q union?
//    top match {
//      case Subquery(p, args, res, sup, current :: rules) => // currently executing a rule
//        current match {
//          case Rule(predicate, params, atoms) =>
//            if (atoms.isEmpty) {
//              // body has no atoms left -> R-result
//            } else {
//              // body has atoms left R-Step which will execute A-Step (executing other rules)
//              nextAtom(top)
//            }
//          case RuleResult(t, s) =>
//            // TODO should we have ruleresult?
//            throw new IllegalStateException(
//              "Rule Result should not occur"
//            )
//        }
//      case Subquery(_, _, _, _, Seq()) =>
//        // Q-Stable? Q-Iterate?
//        queryEnd(top)
//    }
//    top.state match {
//      case QueryState.QueryEntry => ???
//      // rule step
//      case QueryState.AtAtom => ???
//
//      // can be A-Into or A-Over or other simple
//      case QueryState.RuleEnd =>
//      // union
//      // next rule?
//      case QueryState.QueryEnd =>
//        queryEnd(top)
//      case QueryState.QueryResult =>
//        queryResult(top)
//    }
    true
  }

  final protected def ruleMerge(q: Query): Unit = {
    val Subquery(p, args, result, sup, Rule(_, params, atoms) :: rulesTail) = q
    val AtomResult(v, s) = atoms.head
    val atomsTail = atoms.tail
    val nextSup = merge(s, sup, v)
    val next = Subquery(p, args, result, nextSup, Rule(p, params, atomsTail) :: rulesTail)
    queryStack.update(next)
  }

  final protected def ruleResult(q: Query): Unit = {
    val Subquery(p, args, result, sup, Rule(_, params, Nil) :: ruleEvals) = q
    val projected = sup.project(params.map(_.name))
    val next = Subquery(p, args, result, sup, RuleResult(projected) :: ruleEvals)
    queryStack.update(next)
  }

  final def merge(
      sign: TableSign,
      t1: ImmutableTable[Value],
      t2: ImmutableTable[Value]
    ): ImmutableTable[Value] = sign match {
    case PositiveTable => t1.join(t2)
    case NegativeTable => t1.antiJoin(t2)
  }

  // either A-Into, A-Skip or atom rules that process non-call atoms
  final protected def nextAtom(q: Query): Unit = {
    val Subquery(p, args, result, sup, Rule(_, params, atoms) :: rulesTail) = q
    val Atom(atom) = atoms.head
    val atomsTail = atoms.tail
    atom match {
      case Datalog.Call(callee, calleeArgs, _, neg) =>
        val calleeArgBindings = prepareArgBindings(sup, callee, calleeArgs)
        val unseenQueries = state.filterSeenQueries(callee, calleeArgBindings)
        if (unseenQueries.isEmpty) { // A-Skip
          val calleeResult =
            if (isCyclic(callee))
              state.readTopDown(callee, calleeArgBindings)
            else
              state.readBottomUp(callee, calleeArgBindings)
          val calleeParams = predicates(callee).params
          val nextSup =
            if (!neg)
              opJoinBodyAndPred(sup, calleeArgs, calleeParams, calleeResult, (x, y) => x.join(y))
            else
              opJoinBodyAndPred(
                sup,
                calleeArgs,
                calleeParams,
                calleeResult,
                (x, y) => x.antiJoin(y))

          val next = Subquery(p, args, result, nextSup, Rule(p, params, atomsTail) :: rulesTail)
          queryStack.update(next)
        } else { // A-Into
          val calleeParams = predicates(callee).params
          val calleeRules = predicates(callee).bodies.map { body =>
            Rule(callee, calleeParams, body.atoms.map(Atom).toList)
          }.toList
          val next = Subquery(
            callee,
            unseenQueries,
            ImmutableTable.empty(calleeParams.map(_.name)),
            unseenQueries,
            calleeRules)
          queryStack.push(next)
        }

      case _ =>
        val nextSup = atomOps.atom(sup, atom)
        val next = Subquery(p, args, result, nextSup, Rule(p, params, atomsTail) :: rulesTail)
        queryStack.update(next)
    }
  }

  protected def prepareArgBindings(
      t: ImmutableTable[Value],
      p: Predicate,
      args: Seq[Datalog.Term],
      resultIndices: Set[IndexCover] = Set()
    ): ImmutableTable[Value] = {
    val params = predicates(p).params.map(_.name)

    // prepare argsTable
    val paramSubst = params.zip(args)
    val (varsBindings, constBindings) = paramSubst.partition(_._2.isInstanceOf[Datalog.Var])
    val varsBindingsCast = varsBindings.map { case (p, v) => (p, v.asInstanceOf[Datalog.Var]) }
    val constBindingsCast = constBindings.map { case (p, v) =>
      (p, v.asInstanceOf[Datalog.Constant])
    }
    val columnsSubst = varsBindingsCast.map { case (p, v) => (v.name, p) }.toMap

    val argBindings = t.projectAndRename(columnsSubst)

    if (constBindingsCast.isEmpty)
      indexedTableFactory(argBindings.columns, argBindings.entries, resultIndices)
    else {
      // construct constants table
      val constTable = indexedTableFactory(
        constBindingsCast.map(_._1),
        Seq(constBindingsCast.map(x => AtomTableOps.transLiteral(x._2.lit))),
        argBindings)
      // join with constants table
      argBindings.join(constTable, resultIndices)
    }
  }

  private def queryUnion(top: Query): Unit = {
    val Subquery(p, args, result, _, RuleResult(ruleResult, _) :: ruleEvals) = top
    val next = Subquery(p, args, result.union(ruleResult), args, ruleEvals)
    queryStack.update(next)
  }

  private def queryEnd(q: Query): Unit = {
    val Subquery(p, args, result, _, Seq()) = q
    if (isCyclic(p)) {
      state.insertTopDown(p, result)
      state.deleteBlacklist(p, args)
      if (isStable(q)) { // Q-Stable
        queryStable(q)
      } else { // Q-Iterate
        queryIterate(q)
      }
    } else { // non-cyclic predicates are always stable after a single iteration => Q-Stable
      queryStable(q)
    }
  }

  private def isStable(q: Query): Boolean = {
    q match {
      case Subquery(p, args, _, _, ruleEvals) =>
        if (ruleEvals.isEmpty)
          !state.isUnstable(p, args)
        else false
      case QueryResult(_, _, _) => false
    }
  }

  final protected def queryIterate(q: Query): Unit = {
    val Subquery(p, args, result, _, Seq()) = q
    val params = predicates(p).params
    val rules = predicates(p).bodies
    val ruleEvals = rules.map { body =>
      Rule(p, params, body.atoms.map(Atom))
    }
    val next = Subquery(p, args, result, args, ruleEvals)
    state.insertTopDown(p, result)
    queryStack.update(next)
  }

  final protected def queryStable(q: Query): Unit = {
    // pop from stack and replace first atom with atomtable
    val Subquery(callee, _, calleeResult, _, Seq()) = q

    queryStack.pop()
    if (queryStack.isEmpty) {
      queryStack.push(QueryResult(callee, calleeResult))
      return
    }

    val Subquery(caller, callerArgs, callerResult, callerSup, callerRuleEvals) = queryStack.top
    val Rule(_, params, Atom(Datalog.Call(_, calleeTerms, _, neg)) :: atomEvals) =
      callerRuleEvals.head
    val calleeParams = predicates(callee).params
    val columnsSubst = calleeParams.zip(calleeTerms).flatMap {
      case (param, Datalog.Var(argName)) => Some(param.name -> argName)
      case _ => None
    }.toMap

    val renamedColumnsOfPatternTable = calleeResult.columns.flatMap(columnsSubst.get)
    val indexCovers =
      indexedTableFactory.constructIndexCovers(callerSup, renamedColumnsOfPatternTable)

    val v = calleeResult.projectAndRename(columnsSubst, indexCovers)
    val tableSign = if (neg) NegativeTable else PositiveTable
    val nextRuleEval = Rule(caller, params, AtomResult(v, tableSign) :: atomEvals)
    val next =
      Subquery(caller, callerArgs, callerResult, callerSup, nextRuleEval +: callerRuleEvals.tail)
    queryStack.update(next)
  }

  final protected def doStepOverIR(): Boolean = {
    val top = queryStack.top
    val predCyclic = isCyclic(top.predicate)
    if (breakpointHandler.breakpointReachableInPredicate(top, predCyclic)) {
      return false
    }
    top.state match {
      case QueryState.QueryEntry => ???
      case QueryState.RuleEntry => ???
      case QueryState.AtAtom => ???
      case QueryState.RuleEnd => ???
      case QueryState.QueryEnd => ???
    }
  }

  final protected def doStepOutIR(): Boolean = ???

  override def isFinished: Boolean =
    queryStack.size == 1 && queryStack.top.isInstanceOf[QueryResult]
  override def isAtBreakpoint: Boolean =
    // !queryStack.top.isEmpty && breakpointHandler.isAtBreakpoint(queryStack.top)
    breakpointHandler.isAtBreakpoint(queryStack.top)

  private def isCyclic(predicate: String): Boolean = {
    dependencyGraph.cycles.exists(_.contains(predicate))
  }

  private def joinEvalResultAndCall(
      callee: Predicate,
      calleeTable: ImmutableTable[Value]
    ): Unit = {
    val top = queryStack.top
    top match {
      case Subquery(p, args, result, sup, ruleEvals) =>
        val Rule(_, params, atoms) :: remRules = ruleEvals
        val atomsHead :: atomsTail = atoms
        val (calleeArgs, neg) = atomsHead match {
          case Atom(Datalog.Call(_, args, _, neg)) => (args, neg)
          case _ =>
            throw IllegalDebugStateException(
              s"Calling atom has to be indeed a call, but was $atomsHead instead")
        }
        val nextSup =
          if (!neg)
            opJoinBodyAndPred(sup, calleeArgs, params, calleeTable, (x, y) => x.join(y))
          else
            opJoinBodyAndPred(sup, calleeArgs, params, calleeTable, (x, y) => x.antiJoin(y))
        val next = Subquery(p, args, result, nextSup, Rule(p, params, atomsTail) :: remRules)
        queryStack.update(next)
      case _ =>
        throw IllegalDebugStateException("Evaluation result has to be on top of Subquery")
    }
  }

  protected def opJoinBodyAndPred(
      bodyResult: ImmutableTable[Value],
      args: Seq[Datalog.Term],
      params: Seq[Datalog.Param],
      predResult: ImmutableTable[Value],
      op: (ImmutableTable[Value], ImmutableTable[Value]) => ImmutableTable[Value]
    ): ImmutableTable[Value] = {
    // join bodyTable of caller with pattern table of callee
    val columnsSubst = params.zip(args).flatMap {
      case (param, Datalog.Var(argName)) => Some(param.name -> argName)
      case _ => None
    }.toMap

    val renamedColumnsOfPatternTable = predResult.columns.flatMap(columnsSubst.get)
    val indexCovers =
      indexedTableFactory.constructIndexCovers(bodyResult, renamedColumnsOfPatternTable)

    val renamedPatternTable = predResult.projectAndRename(columnsSubst, indexCovers)
    op(bodyResult, renamedPatternTable)
  }
}
