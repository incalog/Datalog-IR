package inca.debugger.redesign

import inca.backend.analyze.DependencyGraph
import inca.backend.ir.Datalog
import inca.compiler.{CompiledDatalogModule, CompiledModule}
import inca.debugger.table.indexing.IndexCover
import inca.debugger.table.ImmutableTable
import inca.debugger.table.IndexedTableFactory
import inca.debugger.DebuggerAPI
import inca.debugger.IllegalDebugStateException
import inca.debugger.Value
import inca.runtime.{DatalogRuntime, EnginePool}
import inca.runtime.context.QueryScope
import inca.runtime.db.DatabaseInput
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

trait Debugger extends DebuggerAPI {

  implicit lazy val indexedTableFactory: IndexedTableFactory[Value] = {
    implicit val valueOrdering: Ordering[Value] = Value.valueOrdering
    val parametersOfRelations = predicates.map { case (name, pat) =>
      name -> pat.params.map(_.name)
    }
    new IndexedTableFactory[Value](parametersOfRelations)
  }

  // ****** global static information ******//
  var module: CompiledDatalogModule = _
  def initialize(module: CompiledDatalogModule): Unit = {
    this.module = CompiledDatalogModule(
      module.ir,
      module.dataModel,
      module.options.withTransformations(module.options.transformations :+ BlacklistTransformation))

  }
  lazy val dependencyGraph: DependencyGraph = new DependencyGraph(module.ir)
  lazy val predicates: Map[Predicate, Datalog.Pattern] = module.ir.patternMap

  def isFinished: Boolean = callStack.size == 1 && callStack.top.isInstanceOf[EvaluationResult]

  def stepped(): Unit = {}

  // TODO make private when finished
  var state: DebuggerState = _
  lazy val atomOps: AtomTableOps = new AtomTableOps(state.bottomUpRuntime, indexedTableFactory)
  val callStack: CallStack = new CallStack

  lazy val breakpointHandler: BreakpointHandler = new BreakpointHandler(dependencyGraph)
  override def isAtBreakpoint: Boolean = !callStack.top.isEmpty && breakpointHandler.isAtBreakpoint(callStack.top)
  override def clearBreakpoints(): Unit = breakpointHandler.clearBreakpoints()

  def initializeDatabaseRuntime(input: DatabaseInput): Unit = {
    val scope = new QueryScope(module.dataModel)
    val (_engine, _database) =
      EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    _engine.delayUpdatePropagation(() => {
      _database.processDatabaseInput(input)
    })
    val rt = DatalogRuntime(_engine, _database, module)
    state = new DebuggerState(rt)
  }

  def entry(p: Predicate, argBindings: ImmutableTable[Value]): Unit = {
    val params = predicates(p).params.map(_.name)
    val next = PredicateEntry(p, argBindings, ImmutableTable.empty(params))
    state.addSeenQuery(p, argBindings)
    callStack.push(next)
    stepped()
  }

  def stepIntoIR(): Boolean = {
    val top = callStack.top
    top match {
      case PredicateEntry(_, _, _) =>
        intoPredicate(top)
      case br@BeforeRule(_, _, _, rules) =>
        if (rules.nonEmpty) intoFirstRule(br)
        else outofPredicate(top)
      case ir@InRule(_, _, _, RuleEvaluation(_, _, atoms), rules) =>
        if (atoms.nonEmpty) nextAtom(top)
        else if (rules.nonEmpty) nextRule(ir)
        else lastRule(top)
      case EvaluationResult(_, _) =>
        evalResult(top)
    }
    stepped()
    true
  }

  final protected def intoFirstRule(evalPoint: BeforeRule): Unit = {
    val BeforeRule(p, argBindings, predResult, rules) = evalPoint
    val rulesHead = rules.head
    val rulesTail = rules.tail
    val next =
      InRule(p, argBindings, predResult, RuleEvaluation(argBindings, 0, rulesHead.atoms), rulesTail)
    callStack.update(next)
  }

  // Step over has the following interesting cases:
  //   - stepping over call
  //     - non-recursive (read DB)
  //     - recursive (read DB with blacklist)
  //   - stepping over predicate entrie
  //     - non-recursive (read DB)
  //     - recursive (read DB with blacklist)
  //   - stepping over rule
  //     - step over all atoms of the rules
  // in all other cases we just do a step-into
  // we also need to consider breakpoints: If breakpoint is reachable we need to resume till breakpoint
  def stepOverIR(): Boolean = {
    val top = callStack.top
    if (breakpointHandler.breakpointReachableInPredicate(top, isCyclic(top.pred)))
      return false

    if (isPredicateEntry(top))
      stepOverPredicate(top)
    else if (isRuleEntry(top))
      stepOverRule(top)
    else if (isPredicateCall(top)) {
      val InRule(_, _, _, RuleEvaluation(_, _, call::_), _) = top
      if (breakpointHandler.breakpointReachableFromCallee(call.asCall.get._1)) {
        // cannot step over
        return false
      } else
        stepOverCall(top)
    } else
      stepIntoIR()
    stepped()
    true
  }

  def stepOutIR(): Boolean = {
    val top = callStack.top
    if (breakpointHandler.breakpointReachableInPredicate(top, isCyclic(top.pred)))
      false
    else if (callStack.size == 1)
      false
    else {
      callStack.pop()
      stepOverCall(callStack.top)
      stepped()
      true
    }
  }

  private def stepOverPredicate(evalPoint: EvaluationPoint): Unit = {
    val PredicateEntry(p, argBindings, _) = evalPoint
    val nextPredResult =
      if (isCyclic(p)) state.readBlacklistedBottomUp(p, argBindings)
      else state.readBottomUp(p, argBindings)
    val next = BeforeRule(p, argBindings, nextPredResult, Seq())
    callStack.update(next)
  }

  protected def stepOverRule(evalPoint: EvaluationPoint): Unit = {
    stepIntoIR()
    evalPoint match {
      case BeforeRule(pred, argBindings, predResult, rules) =>
        val next = InRule(pred, argBindings, predResult, RuleEvaluation(argBindings, 0, rules.head.atoms), rules.tail)
        callStack.update(next)
      case InRule(pred, argBindings, predResult, RuleEvaluation(ruleResult, ruleIdx, _), rules) =>
        val projectedRuleResult = ruleResult.project(predResult.columns)
        val nextPredResult = predResult.union(projectedRuleResult)
        val ruleEval = RuleEvaluation(argBindings, ruleIdx + 1, rules.head.atoms)
        val next = InRule(pred, argBindings, nextPredResult, ruleEval, rules.tail)
        callStack.update(next)
      case _ => // nothing
    }
    var progress = true
    while (progress && callStack.top.asInstanceOf[InRule].current.atoms.nonEmpty) {
      progress = stepOverIR()
    }
    if (!progress)
      resume()
  }

  private def stepOverCall(evalPoint: EvaluationPoint): Unit = {
    val InRule(_, _, _, RuleEvaluation(ruleResult, _, atoms), _) = evalPoint
    val atomsHead = atoms.head
    val (callee, calleeArgs) = atomsHead.asCall.get
    val calleeArgBindings = prepareArgBindings(ruleResult, callee, calleeArgs)
    val calleeResult = state.readBlacklistedBottomUp(callee, calleeArgBindings)
    joinEvalResultAndCall(callee, calleeResult)
  }

  private def isPredicateEntry(evalPoint: EvaluationPoint): Boolean = evalPoint match {
//    case BeforeRule(p, _, _, rules) =>
//      predicates(p).bodies.size == rules.size
    case PredicateEntry(_, _, _) => true
    case _ => false
  }

  // TODO currently there is no difference between rule entry and atom eval point
  private def isRuleEntry(top: EvaluationPoint): Boolean = top match {
    case BeforeRule(_, _, _, rules) if rules.nonEmpty => true
    case InRule(_, _, _, RuleEvaluation(_, _, Nil), rules) if rules.nonEmpty => true
    case _ => false
  }
//    top match {
//      case InRule(p, _, _, RuleEvaluation(_, ruleIdx, atoms), _) =>
//        atoms.size == numberOfAtoms(p, ruleIdx)
//      case _ => false
//    }
//
//  private def numberOfAtoms(p: String, ruleIdx: Int): Int =
//    predicates(p).bodies(ruleIdx).atoms.size

  private def isPredicateCall(top: EvaluationPoint): Boolean = top match {
    case InRule(_, _, _, RuleEvaluation(_, _, atoms), _) =>
      // TODO this will consider recursive aggregation as predicate calls as well
      // Do we want this?
      atoms.nonEmpty && atoms.head.asCall.nonEmpty
    case _ => false
  }

  protected final def intoPredicate(evalPoint: EvaluationPoint): Unit = {
    val PredicateEntry(p, argBindings, predResult) = evalPoint
    val rules = predicates(p).bodies
    if (isCyclic(p)) {
      state.storeExpectedFixpointSize(p, argBindings)
      state.insertBlacklist(p, argBindings)
    }
//    val next =
//      InRule(p, argBindings, predResult, RuleEvaluation(argBindings, 0, rulesHead.atoms), rulesTail)
    val next = BeforeRule(p, argBindings, predResult, rules)
    callStack.update(next)
  }

  protected def outofPredicate(evalPoint: EvaluationPoint): Unit = {
    val BeforeRule(p, argBindings, predResult, _) = evalPoint
    if (isCyclic(p)) {
      state.insertTopDown(p, predResult)
      // we need to delete it it from the blacklist even if we iterate because next E-Predicate will insert it again
      // TODO is there a way to only insert and delete it once?
      state.deleteBlacklist(p, argBindings)
      if (state.isUnstable(p, argBindings)) { // E-Iterate
        val rules = predicates(p).bodies
        val next = BeforeRule(p, argBindings, predResult, rules)
        callStack.update(next)
      } else { // E-Stable
        val next = EvaluationResult(p, predResult)
        callStack.update(next)
      }
    } else { // non-cyclic is always stable, hence E-Stable
      val next = EvaluationResult(p, predResult)
      callStack.update(next)
    }
  }

  final protected def lastRule(evalPoint: EvaluationPoint): Unit = {
    val InRule(p, argBindings, predResult, RuleEvaluation(ruleResult, _, Seq()), Seq()) = evalPoint
    val projectedRuleResult = ruleResult.project(predResult.columns)
    val nextPredResult = predResult.union(projectedRuleResult)
    callStack.update(BeforeRule(p, argBindings, nextPredResult, Seq()))
  }

  final protected def nextRule(evalPoint: InRule): Unit = {
    val InRule(p, argBindings, predResult, RuleEvaluation(ruleResult, ruleIdx, _), rules) =
      evalPoint
    val rulesHead = rules.head
    val rulesTail = rules.tail
    val projectedRuleResult = ruleResult.project(predResult.columns)
    val nextPredResult = predResult.union(projectedRuleResult)
    val ruleEval = RuleEvaluation(argBindings, ruleIdx + 1, rulesHead.atoms)
    val next = InRule(p, argBindings, nextPredResult, ruleEval, rulesTail)
    callStack.update(next)
  }

  protected final def nextAtom(evalPoint: EvaluationPoint): Unit = {
    val InRule(p, argBindings, predResult, RuleEvaluation(ruleResult, ruleIdx, atoms), rules) =
      evalPoint
    val atomsHead = atoms.head
    val atomsTail = atoms.tail
    atomsHead match {
      case Datalog.Call(callee, calleeArgs, _, neg) =>
        val calleeArgBindings = prepareArgBindings(ruleResult, callee, calleeArgs)
        val unseenQueries = state.filterSeenQueries(callee, calleeArgBindings)
        if (unseenQueries.isEmpty) { // E-StepInto-Old
          val calleeResult =
            if (isCyclic(callee))
              state.readTopDown(callee, calleeArgBindings)
            else
              state.readBottomUp(callee, calleeArgBindings)
          val params = predicates(callee).params.map(_.name)
          val nextBodyResult =
            if (!neg)
              opJoinBodyAndPred(ruleResult, calleeArgs, params, calleeResult, (x, y) => x.join(y))
            else
              opJoinBodyAndPred(
                ruleResult,
                calleeArgs,
                params,
                calleeResult,
                (x, y) => x.antiJoin(y))
          val nextRuleEval = RuleEvaluation(nextBodyResult, ruleIdx, atomsTail)
          val next = InRule(p, argBindings, predResult, nextRuleEval, rules)
          callStack.update(next)
        } else { // E-StepInto-New
          val calleeParams = predicates(callee).params.map(_.name)
          val calleeRules = predicates(callee).bodies
          val next = PredicateEntry(callee, unseenQueries, ImmutableTable.empty(calleeParams))
//          val next =
//            BeforeRule(callee, unseenQueries, ImmutableTable.empty(calleeParams), calleeRules)
          callStack.push(next)
        }

      case _ =>
        val nextRuleResult = atomOps.atom(ruleResult, atomsHead)
        val nextRuleEval = RuleEvaluation(nextRuleResult, ruleIdx, atomsTail)
        val next = InRule(p, argBindings, predResult, nextRuleEval, rules)
        callStack.update(next)
    }
  }

  protected final def evalResult(evalPoint: EvaluationPoint): Unit = {
    val EvaluationResult(p, predResult) = evalPoint
    callStack.pop()
    if (callStack.nonEmpty) {
      joinEvalResultAndCall(p, predResult)
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

  protected def opJoinBodyAndPred(
      bodyResult: ImmutableTable[Value],
      args: Seq[Datalog.Term],
      params: Seq[String],
      predResult: ImmutableTable[Value],
      op: (ImmutableTable[Value], ImmutableTable[Value]) => ImmutableTable[Value]
    ): ImmutableTable[Value] = {
    // join bodyTable of caller with pattern table of callee
    val columnsSubst = params.zip(args).flatMap {
      case (param, Datalog.Var(argName)) => Some(param -> argName)
      case _ => None
    }.toMap

    val renamedColumnsOfPatternTable = predResult.columns.flatMap(columnsSubst.get)
    val indexCovers =
      indexedTableFactory.constructIndexCovers(bodyResult, renamedColumnsOfPatternTable)

    val renamedPatternTable = predResult.projectAndRename(columnsSubst, indexCovers)
    op(bodyResult, renamedPatternTable)
  }

  private def joinEvalResultAndCall(
      callee: Predicate,
      calleeTable: ImmutableTable[Value]
    ): Unit = {
    val top = callStack.top
    top match {
      case InRule(p, argBindings, predResult, RuleEvaluation(bodyResult, ruleIdx, atoms), rules) =>
        val atomsHead = atoms.head
        val atomsTail = atoms.tail
        val params = predicates(callee).params.map(_.name)
        val (calleeArgs, neg) = atomsHead match {
          case Datalog.Call(_, args, _, neg) => (args, neg)
          case _ =>
            throw IllegalDebugStateException(
              s"Calling atom has to be indeed a call, but was $atomsHead instead")
        }
        val nextBodyResult =
          if (!neg)
            opJoinBodyAndPred(bodyResult, calleeArgs, params, calleeTable, (x, y) => x.join(y))
          else
            opJoinBodyAndPred(bodyResult, calleeArgs, params, calleeTable, (x, y) => x.antiJoin(y))
        val nextRuleEval = RuleEvaluation(nextBodyResult, ruleIdx, atomsTail)
        val next = InRule(p, argBindings, predResult, nextRuleEval, rules)
        callStack.update(next)
      case _ =>
        throw IllegalDebugStateException(
          "Evaluation result has to be on top of InRule evaluation point")
    }
  }

  private def isCyclic(predicate: Predicate): Boolean =
    dependencyGraph.cycles.exists(_.contains(predicate))

  def varsIR: ImmutableTable[Value] = callStack.top match {
    case PredicateEntry(pred, argBindings, predResult) => argBindings
    case BeforeRule(pred, argBindings, predResult, rules) => argBindings
    case InRule(pred, argBindings, predResult, current, remainingRules) => current.ruleResult
    case EvaluationResult(pred, predResult) => predResult
  }
}
