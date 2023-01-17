package inca.debugger.redesign_new

import inca.backend.analyze.DependencyGraph
import inca.backend.ir.Datalog
import inca.backend.optimize.InlineSimpleRelations
import inca.compiler.CompiledDatalogModule
import inca.compiler.CompiledModule
import inca.debugger.table.indexing.IndexCover
import inca.debugger.table.IndexedTableFactory
import inca.debugger.DebuggerAPI
import inca.debugger.Value
import inca.runtime.context.QueryScope
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import inca.runtime.EnginePool
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import scala.collection.mutable

/**
 */
trait Debugger extends DebuggerAPI {
  /*
   * Global static information
   */
  private var module: CompiledDatalogModule = _
  private lazy val dependencyGraph = new DependencyGraph(module.ir)
  protected lazy val preds: Map[Predicate, Datalog.Pattern] = module.ir.patternMap
  private lazy val predParams = preds.map { case (name, pat) => name -> pat.params.map(_.name) }
  private lazy val atomOps: AtomTableOps = new AtomTableOps(state.bottomUpRuntime, tableFactory)
  implicit private lazy val tableFactory: IndexedTableFactory[Value] = {
    implicit val valueOrdering: Ordering[Value] = Value.valueOrdering
    new IndexedTableFactory[Value](predParams)
  }

  /*
   * Debugger state
   */
  protected[redesign_new] var state: DebuggerState = _
  protected[redesign_new] val queryStack: QueryStack = new QueryStack
  protected[redesign_new] lazy val breakpointHandler: BreakpointHandler = new BreakpointHandler(
    dependencyGraph)

  /*
   * This method initializes the debugger. It is required to call this method before using the debugger
   */
  def initialize(module: CompiledModule): Unit = {
    if (module.options.optimizations.contains(InlineSimpleRelations))
      throw new IllegalArgumentException(s"Inlining must be deactivated for debugging.")

    val trans = module.options.transformations :+ BlacklistTransformation
    val options = module.options.withTransformations(trans)
    this.module = CompiledDatalogModule(module.ir, module.dataModel, options)
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

  /*
   * Functions reading the debugging state
   */
  override def isFinished: Boolean =
    queryStack.size == 1 && queryStack.top.isInstanceOf[QueryResult]

  override def isAtBreakpoint: Boolean =
    // !queryStack.top.isEmpty && breakpointHandler.isAtBreakpoint(queryStack.top)
    breakpointHandler.isAtBreakpoint(queryStack.top)

  /*
   * This method sets the entry point when using the debugger.
   * @pred Marks the predicate
   * @args States the argument table for the entry predicate
   */
  def entry(pred: Predicate, args: ValueTable): Unit = {
    // clear state and querystack
    state.clear()
    queryStack.clear()

    val params = predParams(pred)
    val rules = preds(pred).bodies.map { body => Rule(pred, params, body.atoms.map(Atom)) }
    val result = ValueTable.empty(params)
    val query = Subquery(pred, args, result, args, rules)
    state.storeExpectedFixpointSize(pred, args, queryStack.size + 1)
    state.insertBlacklist(pred, args)
    queryStack.push(query)
    stepped()
  }

  /*
   * Reduction functions by the formal semantics
   */
  private def ruleReduction(
      sup: ValueTable,
      rule: RuleEval,
      stepOver: Boolean
    ): (RuleEval, ValueTable) = rule match {
    case Rule(p, params, atoms @ Atom(_) :: _) => // R-Step
      val nextAtom = atomReduction(sup, atoms.head, stepOver)
      (Rule(p, params, nextAtom +: atoms.tail), sup)

    case Rule(p, params, atoms @ AtomResult(v, s) :: _) => // R-Merge
      val nextSup = tableMerge(s, sup, v)
      (Rule(p, params, atoms.tail), nextSup)

    case Rule(_, params, Nil) => // R-Result
      val projected = sup.project(params)
      (RuleResult(projected), sup)
  }

  private def atomReduction(sup: ValueTable, atomEval: AtomEval, stepOver: Boolean): AtomEval = {
    val Atom(atom) = atomEval
    atom match {
      case call @ Datalog.Call(callee, calleeTerms, _, neg) =>
        val calleeArgs = prepareArgTable(sup, callee, calleeTerms)
        if (stepOver) { // A-Over and A-OverRecursive
          val bottomUpTable = state.readBlacklistedBottomUp(callee, calleeArgs)
          val result =
            if (isCyclic(callee)) bottomUpTable.union(state.readTopDown(callee, calleeArgs))
            else bottomUpTable
          val table = fitToSupplementary(callee, calleeTerms, result, sup)
          val sign = if (neg) NegativeTable else PositiveTable
          AtomResult(table, sign)
        } else {
          // A-Into or A-Skip
          val unseenQueries = state.addNewQuery(callee, calleeArgs)
          if (unseenQueries.nonEmpty) { // A-Into
            atomInto(callee, unseenQueries)
            atomEval
          } else { // A-Skip
            atomSkip(sup, calleeArgs, call)
          }
        }
      case _ =>
        if (stepOver) {
          throw new IllegalArgumentException("Cannot step-over a non-call atom")
        }
        // this design is following the formal semantics but is inefficient
        // TODO we want to avoid unnecessary joins and directly process and change the supplementary
        val nextSup = atomOps.atom(sup, atom)
        AtomResult(nextSup, PositiveTable)
    }
  }

  // has a side-effect as it pushes a new subquery onto the query stack
  private def atomInto(pred: Predicate, args: ValueTable): Unit = {
    val params = predParams(pred)
    val rules = preds(pred).bodies
    val ruleEvals = rules.map { body => Rule(pred, params, body.atoms.map(Atom)) }
    if (isCyclic(pred)) {
      state.storeExpectedFixpointSize(pred, args, queryStack.size + 1)
      state.insertBlacklist(pred, args)
    }
    val emptyResult = ValueTable.empty(params)
    val newSubquery = Subquery(pred, args, emptyResult, args, ruleEvals)
    queryStack.push(newSubquery)
  }

  private def atomSkip(sup: ValueTable, args: ValueTable, call: Datalog.Call): AtomEval = {
    val calleeResult =
      if (isCyclic(call.name)) state.readTopDown(call.name, args)
      else state.readBottomUp(call.name, args)
    val result = fitToSupplementary(call.name, call.args, calleeResult, sup)
    val sign = if (call.neg) NegativeTable else PositiveTable
    AtomResult(result, sign)
  }

  private def queryReduction(query: Query, stepOver: Boolean): Query = query match {
    case Subquery(pred, args, result, sup, rules @ Rule(_, _, _) +: _) => // Q-Step
      val (nextRule, nextSup) = ruleReduction(sup, rules.head, stepOver)
      Subquery(pred, args, result, nextSup, nextRule +: rules.tail)

    case Subquery(pred, args, result, _, rules @ RuleResult(ruleResult) +: _) => // Q-Union
      Subquery(pred, args, result.union(ruleResult), args, rules.tail)

    case Subquery(pred, args, result, _, Nil) =>
      if (isCyclic(pred)) {
        if (isStable(query)) { // Q-Stable
          queryStable(query)
        } else { // Q-Iterate
          val params = predParams(pred)
          val rules = preds(pred).bodies
          val ruleEvals = rules.map { body => Rule(pred, params, body.atoms.map(Atom)) }
          // Q-Iterate will only be called for non-cyclic predicates
          // hence we only store top-down derived tuples for cyclic predicates
          state.insertTopDown(pred, result)
          Subquery(pred, args, result, args, ruleEvals)
        }
      } else {
        // non-cyclic predicates are always stable after a single iteration => Q-Stable
        // this optimization is not present in the formal semantics
        queryStable(query)
      }
    case QueryResult(_, result) =>
      // This rule is not present in the formal semantics
      // It is required because we use a querystack instead of nested subqueries
      queryStack.pop()
      replaceCallWithAtomResult(queryStack.top, result)
  }

  private def queryStable(q: Query): Query = {
    val Subquery(pred, args, result, _, Seq()) = q
    // we insert when the query is stable because we avoid a non-producing iteration
    if (isCyclic(pred)) {
      state.insertTopDown(pred, result)
    }
    state.deleteBlacklist(pred, args)
    QueryResult(pred, result)
  }

  /*
   * Helper Functions
   */
  private def tableMerge(sign: TableSign, t1: ValueTable, t2: ValueTable): ValueTable =
    sign match {
      case PositiveTable => t1.join(t2)
      case NegativeTable => t1.antiJoin(t2)
    }

  private def isStable(query: Query): Boolean = query match {
    case Subquery(p, args, result, _, Nil) => state.isStable(p, args, result, queryStack.size)
    case _ => false
  }

  private def isCyclic(pred: Predicate): Boolean = dependencyGraph.cycles.exists(_.contains(pred))

  // similar to the helper function eval in the paper
  private def prepareArgTable(
      t: ValueTable,
      pred: Predicate,
      args: Seq[Datalog.Term],
      resultIndices: Set[IndexCover] = Set()
    ): ValueTable = {
    val params = predParams(pred)
    val (varSubst, constSubst) = params.zip(args).partitionMap {
      case (n, v: Datalog.Var) => Left(n -> v)
      case (n, c: Datalog.Constant) => Right(n -> c)
    }
    val columnsSubst = varSubst.map { case (p, v) => (v.name, p) }.toMap
    val varTable = t.projectAndRename(columnsSubst)

    val constEntry = Seq(constSubst.map(x => AtomTableOps.transLiteral(x._2.lit)))
    val constTable = tableFactory(constSubst.map(_._1), constEntry, varTable)

    varTable.join(constTable, resultIndices)
  }

  private def replaceCallWithAtomResult(query: Query, calleeResult: ValueTable): Query = {
    val Subquery(pred, args, result, sup, rules) = query
    val Rule(_, params, atoms) = rules.head
    val Atom(Datalog.Call(callee, calleeTerms, _, neg)) = atoms.head
    val atomTable = fitToSupplementary(callee, calleeTerms, calleeResult, sup)
    val tableSign = if (neg) NegativeTable else PositiveTable
    val rule = Rule(pred, params, AtomResult(atomTable, tableSign) +: atoms.tail)
    Subquery(pred, args, result, sup, rule +: rules.tail)
  }

  private def fitToSupplementary(
      pred: Predicate,
      terms: Seq[Datalog.Term],
      result: ValueTable,
      sup: ValueTable
    ): ValueTable = {
    val params = predParams(pred)
    val columnsSubst = params.zip(terms).flatMap {
      case (param, Datalog.Var(argName)) => Some(param -> argName)
      case _ => None
    }.toMap
    val renamedColumns = result.columns.flatMap(columnsSubst.get)
    val indexCovers = tableFactory.constructIndexCovers(sup, renamedColumns)
    result.projectAndRename(columnsSubst, indexCovers)
  }

  private def reduce(stepOver: Boolean, shortCircuit: Boolean): Boolean = {
    val query = queryStack.top
    val stackHeight = queryStack.size
    // we check if breakpoint is reachable when using step over, hence we want to resume instead
    if (stepOver && breakpointHandler.stepOverReachesBreakpoint(query))
      return false
    val newQuery = queryReduction(query, stepOver)
    val shortCircuitedQuery =
      if (shortCircuit) shortCircuitIfPossible(newQuery)
      else newQuery
    if (stackHeight > queryStack.size) {
      // queryReduction popped from stack (queryresult rule)
      queryStack.update(shortCircuitedQuery)
    } else {
      // this is needed because A-Into pushes to the stack
      // hence we would overwrite the new subquery and not the original one
      queryStack.update(shortCircuitedQuery, stackHeight)
    }
    true
  }

  private def shortCircuitIfPossible(query: Query): Query = query match {
    case Subquery(pred, args, result, sup, _ :: rules) =>
      val params = predParams(pred)
      val ruleResult = ValueTable.empty(params)
      Subquery(pred, args, result, sup, RuleResult(ruleResult) +: rules)
    case _ => query
  }

  final protected def doStepIntoIR(shortCircuit: Boolean): Boolean =
    reduce(stepOver = false, shortCircuit)

  final protected def doStepOverIR(shortCircuit: Boolean): Boolean =
    reduce(stepOver = true, shortCircuit)

  final protected def doStepOutIR(shortCircuit: Boolean): Boolean = {
    val query = queryStack.top
    val isPredCyclic = isCyclic(query.pred)
    if (breakpointHandler.stepOutReachesBreakpoint(query, isPredCyclic))
      return false
    queryStack.top match {
      case Subquery(pred, args, _, _, _) =>
        state.deleteBlacklist(pred, args)
        val bottomUpResult = state.readBlacklistedBottomUp(pred, args)
        queryStack.update(QueryResult(pred, bottomUpResult))
      case QueryResult(_, _) =>
      // do nothing
    }
    true
  }
}
