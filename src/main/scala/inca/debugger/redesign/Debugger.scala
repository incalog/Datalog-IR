package inca.debugger.redesign

import inca.backend.analyze.DependencyGraph
import inca.backend.ir.Datalog
import inca.debugger.table.indexing.IndexCover
import inca.debugger.table.ImmutableTable
import inca.debugger.table.IndexedTableFactory
import inca.debugger.DebuggerAPI
import inca.debugger.IllegalDebugStateException
import inca.debugger.Value
import inca.runtime.DatalogRuntime

abstract class Debugger(module: Datalog.Module) extends DebuggerAPI {

  implicit lazy val indexedTableFactory: IndexedTableFactory[Value] = {
    implicit val valueOrdering: Ordering[Value] = Value.valueOrdering
    val parametersOfRelations = predicates.map { case (name, pat) =>
      name -> pat.params.map(_.name)
    }
    new IndexedTableFactory[Value](parametersOfRelations)
  }

  // ****** global static information ******//
  lazy val dependencyGraph: DependencyGraph = new DependencyGraph(module)
  lazy val predicates: Map[Predicate, Datalog.Pattern] = module.patternMap

  def isFinished: Boolean = callStack.size == 1 && callStack.top.isInstanceOf[EvaluationResult]

  // TODO make private when finished
  var state: DebuggerState = _
  lazy val atomOps: AtomTableOps = new AtomTableOps(state.bottomUpRuntime, indexedTableFactory)
  val callStack: CallStack = new CallStack

  def setBottomUpRuntime(rt: DatalogRuntime): Unit = {
    state = new DebuggerState(rt)
  }

  def entry(p: Predicate, argBindings: ImmutableTable[Value]): Unit = {
    val params = predicates(p).params.map(_.name)
    val bodies = predicates(p).bodies
    val next = BeforeRule(p, argBindings, ImmutableTable.empty(params), bodies)
    state.addSeenQuery(p, argBindings)
    callStack.push(next)
  }

  def stepInto(): Unit = {
    val top = callStack.top
    top match {
      case BeforeRule(_, _, _, _ :: _) =>
        intoPredicate(top)
      case BeforeRule(_, _, _, Nil) =>
        outofPredicate(top)
      case InRule(_, _, _, RuleEvaluation(_, Nil), Nil) =>
        lastRule(top)
      case InRule(_, _, _, RuleEvaluation(_, Nil), _ :: _) =>
        nextRule(top)
      case InRule(_, _, _, RuleEvaluation(_, _ :: _), _) =>
        nextAtom(top)
      case EvaluationResult(_, _) =>
        evalResult(top)
    }
  }

  private def intoPredicate(evalPoint: EvaluationPoint): Unit = {
    val BeforeRule(p, argBindings, predResult, rule :: remRules) = evalPoint
    if (isCyclic(p)) {
      state.storeExpectedFixpointSize(p, argBindings)
      state.insertBlacklist(p, argBindings)
    }
    val next = InRule(p, argBindings, predResult, RuleEvaluation(argBindings, rule.atoms), remRules)
    callStack.update(next)
  }

  private def outofPredicate(evalPoint: EvaluationPoint): Unit = {
    val BeforeRule(p, argBindings, predResult, Nil) = evalPoint
    if (isCyclic(p)) {
      state.insertTopDown(p, predResult)
      // we need to delete it it from the blacklist even if we iterate because next E-Predicate will insert it again
      // TODO is there a way to only insert and delete it once?
      state.deleteBlacklist(p, argBindings)
      if (state.isUnstable(p, argBindings)) { // E-Iterate
        println("ITERATE")
        val params = predicates(p).params.map(_.name)
        val rules = predicates(p).bodies
        val next = BeforeRule(p, argBindings, ImmutableTable.empty(params), rules)
        callStack.update(next)
      } else { // E-Stable
        println("STABLE")
        val next = EvaluationResult(p, predResult)
        callStack.update(next)
      }
    } else { // non-cyclic is always stable, hence E-Stable
      val next = EvaluationResult(p, predResult)
      callStack.update(next)
    }
  }

  private def lastRule(evalPoint: EvaluationPoint): Unit = {
    val InRule(p, argBindings, predResult, RuleEvaluation(ruleResult, Nil), Nil) = evalPoint
    val projectedRuleResult = ruleResult.project(predResult.columns)
    val nextPredResult = predResult.union(projectedRuleResult)
    callStack.update(BeforeRule(p, argBindings, nextPredResult, Nil))
  }

  private def nextRule(evalPoint: EvaluationPoint): Unit = {
    val InRule(p, argBindings, predResult, RuleEvaluation(ruleResult, Nil), nextRule :: remRules) =
      evalPoint
    val nextPredResult = predResult.union(ruleResult)
    val ruleEval = RuleEvaluation(argBindings, nextRule.atoms)
    val next = InRule(p, argBindings, nextPredResult, ruleEval, remRules)
    callStack.update(next)
  }

  private def nextAtom(evalPoint: EvaluationPoint): Unit = {
    val InRule(p, argBindings, predResult, RuleEvaluation(ruleResult, atom :: remAtoms), remRules) =
      evalPoint
    atom match {
      case Datalog.Call(callee, calleeArgs, _, neg) =>
        val calleeArgBindings = prepareArgBindings(ruleResult, callee, calleeArgs)
        val unseenQueries = state.filterSeenQueries(p, calleeArgBindings)
        if (unseenQueries.isEmpty) { // E-StepInto-Old
          println(s"SEEN $callee with $calleeArgBindings")
          val calleeResult =
            if (isCyclic(p))
              state.readTopDown(p, calleeArgBindings)
            else
              state.readBottomUp(p, calleeArgBindings)
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
          val nextRuleEval = RuleEvaluation(nextBodyResult, remAtoms)
          val next = InRule(p, calleeArgBindings, predResult, nextRuleEval, remRules)
          callStack.update(next)
        } else { // E-StepInto-New
          val calleeParams = predicates(callee).params.map(_.name)
          val rules = predicates(callee).bodies
          val next = BeforeRule(callee, unseenQueries, ImmutableTable.empty(calleeParams), rules)
          callStack.push(next)
        }

      case _ =>
        val nextRuleResult = atomOps.atom(ruleResult, atom)
        val nextRuleEval = RuleEvaluation(nextRuleResult, remAtoms)
        val next = InRule(p, argBindings, predResult, nextRuleEval, remRules)
        callStack.update(next)
    }
  }

  private def evalResult(evalPoint: EvaluationPoint): Unit = {
    val EvaluationResult(p, predResult) = evalPoint
    callStack.pop()
    if (callStack.nonEmpty) {
      println("RESULT")
      joinEvalResultAndCall(p, predResult)
    }
  }

  // TODO predicate entry should also step over
  // TODO step over body should run step over till end of body reached
  // TODO rewritte program instead of implementing blacklist transformation
  def stepOver(): Unit = callStack.top match {
    case InRule(_, _, _, RuleEvaluation(ruleResult, atom :: _), _) =>
      if (atom.asCall.nonEmpty) {
        // do different things based on if predicate is cyclic
        val (callee, calleeArgs) = atom.asCall.get
        val calleeArgBindings = prepareArgBindings(ruleResult, callee, calleeArgs)
        val calleeResult = state.readBlacklistedBottomUp(callee, calleeArgBindings)
        joinEvalResultAndCall(callee, calleeResult)
      } else stepInto()
    case _ => stepInto()
  }

  private def prepareArgBindings(
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

  private def opJoinBodyAndPred(
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
      case InRule(
            p,
            argBindings,
            predResult,
            RuleEvaluation(bodyResult, atom :: remAtoms),
            remRules) =>
        val params = predicates(callee).params.map(_.name)
        val (calleeArgs, neg) = atom match {
          case Datalog.Call(_, args, _, neg) => (args, neg)
          case _ =>
            throw IllegalDebugStateException(
              s"Calling atom has to be indeed a call, but was $atom instead")
        }
        val nextBodyResult =
          if (!neg)
            opJoinBodyAndPred(bodyResult, calleeArgs, params, calleeTable, (x, y) => x.join(y))
          else
            opJoinBodyAndPred(bodyResult, calleeArgs, params, calleeTable, (x, y) => x.antiJoin(y))
        val nextRuleEval = RuleEvaluation(nextBodyResult, remAtoms)
        val next = InRule(p, argBindings, predResult, nextRuleEval, remRules)
        callStack.update(next)
      case _ =>
        throw IllegalDebugStateException(
          "Evaluation result has to be on top of InRule evaluation point")
    }
  }

  private def isCyclic(predicate: Predicate): Boolean =
    dependencyGraph.cycles.exists(_.contains(predicate))

  // TODO
  override def stepOut(): Unit = ???
  override def resume(): Unit = ???
  override def resumeWithStepInto(): Unit = ???
  override type Breakpoint = String
  override def addBreakpoint(bp: Breakpoint): Unit = ???
  override def removeBreakpoint(bp: Breakpoint): Unit = ???
  override def clearBreakpoints(): Unit = ???
}
