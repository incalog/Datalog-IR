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

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

trait Debugger extends DebuggerAPI {
  // Datalog program information
  private var compiled: CompiledModule = _
  protected lazy val patterns: Map[String, Datalog.Pattern] =
    compiled.ir.pats.map { pat => pat.name -> pat }.toMap
  protected lazy val dependencyGraph = new DependencyGraph(compiled.ir)

  // Extensional database stuff
  protected var database: Database = _
  private var engine: AdvancedViatraQueryEngine =  _

  // Debugger state
  private lazy val fixpointState: FixpointState[Value] = new FixpointState[Value](patterns)
  protected val callStack: CallStack = new CallStack()
  private val _controlTrace: ListBuffer[ControlPoint] = ListBuffer.empty

  // we store the derived tuples for a given pattern before executing the pattern to check if we reached a fixpoint
  private var lastDerivedTuples: Table[Value] = _

  // Needed to execute scala code via reflection
  protected val scalaCompiler = new Scala.ScalaCompiler()
  protected var defintionObjSym: String = _

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
    initScalaCompiler()
  }

  def updateExtensionalData(edits: EditScript): Unit =
    engine.delayUpdatePropagation { () =>
      database.processEditScript(edits)
    }

  private def initScalaCompiler(): Unit = {
    defintionObjSym = scalaCompiler.define {
      import scala.meta._
      q"object O {..${compiled.psystemSource.stats}}".syntax
    }
  }

  // Debugger methods
  def entry(name: Datalog.Name, bindings: Table[Value]): Unit = {
    val pat = patterns(name)
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
      val nextBodyTable = transitionAtomTables(frame, atom)
      val next = frame.cp.stepIntra.get // yields next atom
      callStack.update(frame.copy(cp = next, bodyTable = nextBodyTable))
    }
    abortIfBodyFailed()
  }

  protected def stepIntoIRCall(frame: Frame, atom: Datalog.Atom): Unit = atom match {
    case Datalog.Call(name, args, _, neg) =>
      val pattern = patterns(name)
      val argsTable = prepareArgTableOfCall(frame, pattern, args)
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
          val patternTable = transitionReturnCallTables(frame, name, argsTable, neg)
          val next = frame.cp.stepOver.get
          callStack.update(Frame(next, patternTable))
      }
    case Datalog.Computed(lhs, agg: Datalog.CountAggregation) =>
      val pattern = patterns(agg.patName)
      val argTable = prepareArgTableOfCall(frame, pattern, agg.args)
      fixpointState.addQuery(agg.patName, argTable) match {
        case Some(query) =>
          val callee = ControlPoint(PatternPoint(pattern, BeforeList))
          callStack.push(Frame(callee, query, query))
        case None =>
          val patternTable = fixpointState.relation(agg.patName, argTable)
          val next = frame.cp.stepIntra.getOrElse(throw IllegalDebugStateException("Cannot have non-atom frame below pattern end frame on call stack"))
          val tables = transitionCountAggTables(frame, patternTable, lhs)
          callStack.update(Frame(next, tables))
      }
    case Datalog.Computed(lhs, agg: Datalog.CustomAggregation) =>
      val pattern = patterns(agg.patName)
      val argTable = prepareArgTableOfCall(frame, pattern, agg.args)
      fixpointState.addQuery(agg.patName, argTable) match {
        case Some(query) =>
          val callee = ControlPoint(PatternPoint(pattern, BeforeList))
          callStack.push(Frame(callee, query, query))
        case None =>
          val patternTable = fixpointState.relation(agg.patName, argTable)
          val next = frame.cp.stepIntra.getOrElse(throw IllegalDebugStateException("Cannot have non-atom frame below pattern end frame on call stack"))
          val tables = transitionCustomAggTables(frame, patternTable, lhs, agg)
          callStack.update(Frame(next, tables))
      }
    case _ => throw IllegalDebugStateException(s"Atom $atom should contain call")
  }

  private def transitionReturnCallTables(callerFrame: Frame, name: String, argsTable: Table[Value], neg: Boolean = false): Frame.Tables = {
    val patternTable = fixpointState.relation(name, argsTable)
    val params = patterns(name).params.map(_.name)
    if (neg)
      transitionReturnNegCallTables(callerFrame, params, patternTable)
    else {
      transitionReturnCallTables(callerFrame, params, patternTable)
    }
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
          transitionReturnCallTables(callerFrame, frame)
        case Datalog.Call(_, _, _, true) =>
          transitionReturnNegCallTables(callerFrame, frame)
        case Datalog.Computed(lhs, custAgg: CustomAggregation) =>
          val patternTable = fixpointState.relation(custAgg.patName, frame.argsTable)
          transitionCustomAggTables(callerFrame, patternTable, lhs, custAgg)
        case Datalog.Computed(lhs, countAgg: CountAggregation) =>
          val patternTable = fixpointState.relation(countAgg.patName, frame.argsTable)
          transitionCountAggTables(callerFrame, patternTable, lhs)
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

  // Methods to prepare frame tables for atoms that can jump into another pattern (calls and aggregations)
  protected def prepareArgTableOfCall(frame: Frame, calledPattern: Datalog.Pattern, args: Seq[Datalog.Term]): Table[Value] = {
    val params = calledPattern.params.map(_.name)

    // prepare argsTable
    val paramSubst = params.zip(args)
    val (varsBindings, constBindings) = paramSubst.partition(_._2.isInstanceOf[Datalog.Var])
    val varsBindingsCast = varsBindings.map { case (p, v) => (p, v.asInstanceOf[Datalog.Var]) }
    val constBindingsCast = constBindings.map { case (p, v) => (p, v.asInstanceOf[Datalog.Constant]) }
    val columnsSubst = varsBindingsCast.map { case (p, v) => (v.name, p) }.toMap
    val projected = frame.bodyTable.project(varsBindingsCast.map(_._2.name))
    var argsTable = projected.renameColumns(columnsSubst)
    constBindingsCast.foreach { case (p, c) =>
      argsTable = argsTable.bind(p, transLiteral(c.lit))
    }

    argsTable
  }


  // methods to prepare frame tables for atoms that do not jump into another pattern (atom is not a call, or aggregation)
  private def transitionAtomTables(frame: Frame, atom: Datalog.Atom): Table[Value] = atom match {
    case ht: Datalog.HasType =>
      transitionHasTypeTables(frame, ht)
    case nht: Datalog.NotHasType =>
      transitionNotHasTypeTables(frame, nht)
    case comp@Datalog.Compare(Datalog.EqComparator, _, _) =>
      transitionEqCompTables(frame, comp)
    case comp@Datalog.Compare(Datalog.NeqComparator, _, _) =>
      transitionNeqCompTables(frame, comp)
    case p: Datalog.Path =>
      transitionPathTables(frame, p)
    case np: Datalog.NoPath =>
      transitionNoPathTables(frame, np)
    case un: Datalog.Undef =>
      transitionUndefTables(frame, un)
    case ext: Datalog.ExtensionalCall =>
      transitionExtCallTables(frame, ext)
    case Datalog.Computed(lhs, eval: Datalog.Evaluation) =>
      transitionEvalTables(frame, lhs, eval)
    case Datalog.Call(_, _, _, _) =>
      throw IllegalDebugStateException(s"The function transitionAtomTables should not be called with call atom $atom")
    case Datalog.Computed(_, _) =>
      throw IllegalDebugStateException(s"The function transitionAtomTables should not be called with computed atom $atom")
  }


  // method to prepare frame tables of atoms that query unary edb relations (has type and not has type)
  private def transitionHasTypeTables(frame: Frame, ht: Datalog.HasType): Table[Value] =
    ht.t match {
      case Datalog.Var(name) =>
        val key = NodeTypeKey(transType(ht.typ))
        transitionUnaryIndexTable(frame.bodyTable, name, key)
      case Datalog.Constant(_) =>
        throw new IllegalArgumentException(s"HasType is not defined on constants $ht")
    }

  private def transType(typ: Datalog.Type): truechange.Type = typ match {
    case Datalog.TAny => truechange.AnyType
    case Datalog.TNode(name) => truechange.SortType(name)
    case Datalog.TList(ty) => truechange.ListType(transType(ty))
    case _ => throw new IllegalArgumentException("NOT SUPPORTED YET")
  }

  private def transitionNotHasTypeTables(frame: Frame, nht: Datalog.NotHasType): Table[Value] =
    nht.t match {
      case Datalog.Var(name) =>
        val key = NotNodeTypeIndex.Key(transType(nht.typ))
        transitionUnaryIndexTable(frame.bodyTable, name, key)
      case Datalog.Constant(_) =>
        throw new IllegalArgumentException(s"HasType is not defined on constants $nht")
    }

  private def transitionUnaryIndexTable(table: Table[Value], col: String, key: IInputKey): Table[Value] = {
    if (table.isBound(col)) {
      val colIdx = table.columnIndex(col)
      table.filter { row =>
        val v = row(colIdx).unwrap
        database.containsTuple(key, Tuples.staticArityFlatTupleOf(v))
      }
    } else {
      val vals = database.enumerateValues(key, TupleMask.empty(0), Tuples.staticArityFlatTupleOf()).asScala
      val nameTable = Table[Value](Seq(col), vals.map(v => Seq(Value(v))))
      table.join(nameTable)
    }
  }


  // methods to prepare frame tables for atoms that query binary edb relations (path, nopath)
  private def transitionPathTables(frame: Frame, p: Datalog.Path): Table[Value] = {
    val (src, trg) = (p.src, p.trg) match {
      case (Datalog.Var(srcName), Datalog.Var(trgName)) => (srcName, trgName)
      case _ => throw IllegalDebugStateException(s"Path is not defined on constants $p")
    }
    val key = generateLinkKey(p.link)
    val bodyTable = frame.bodyTable
    val nextBodyTable =
      if (bodyTable.isBound(src) && bodyTable.isBound(trg))
        transitionBinaryIndexQueryBothBound(bodyTable, key, src, trg)
      else if (bodyTable.isBound(src) && !bodyTable.isBound(trg))
        transitionBinaryIndexQueryOneBound(bodyTable, key, src, trg, isSourceBound = true)
      else if (!bodyTable.isBound(src) && bodyTable.isBound(trg))
        transitionBinaryIndexQueryOneBound(bodyTable, key, trg, src, isSourceBound = false)
      else
        transitionBinaryIndexQueryUnbound(bodyTable, key, trg, src)
    nextBodyTable
  }

  private def transitionBinaryIndexQueryBothBound(table: Table[Value], key: IInputKey, src: String, trg: String): Table[Value] = {
    val idxL = table.columnIndex(src)
    val idxR = table.columnIndex(trg)
    table.filter { row =>
      val vL = row(idxL).unwrap
      val vR = row(idxR).unwrap
      database.containsTuple(key, Tuples.staticArityFlatTupleOf(vL, vR))
    }
  }

  private def transitionBinaryIndexQueryOneBound(table: Table[Value], key: IInputKey, bound: String, unbound: String, isSourceBound: Boolean): Table[Value] = {
    val selectIdx = if (isSourceBound) 0 else 1
    val mask = TupleMask.selectSingle(selectIdx, 2)
    val boundIdx = table.columnIndex(bound)
    table.expand(unbound, { row =>
      val boundV = row(boundIdx).unwrap
      val unboundURI = database.enumerateValues(key, mask, Tuples.staticArityFlatTupleOf(boundV)).iterator().next()
      Value(unboundURI)
    })
  }

  private def transitionBinaryIndexQueryUnbound(table: Table[Value], key: IndexKey[_], src: String, trg: String): Table[Value] = {
    val rows = database.enumerateTuples(key, TupleMask.empty(2), Tuples.staticArityFlatTupleOf()).iterator().asScala.map { tuple =>
      val vL = tuple.get(0)
      val vR = tuple.get(1)
      Seq(Value(vL), Value(vR))
    }
    val srcTrgTable = Table(Seq(src, trg), rows.toSeq)
    table.join(srcTrgTable)
  }

  private def generateLinkKey(link: Datalog.Link): IndexKey[_] = link match {
    case Datalog.ParentLink => ParentIndex.Key
    case Datalog.NextLink => LinkListNextKey
    case Datalog.SizeLink => SizeIndex.Key
    case Datalog.NamedLink(node, field) =>
      val link = (node.name, field)
      if (compiled.dataModel.links.contains(link))
        LinkNodeKey(link)
      else
        LinkPrimitiveKey(link)
  }

  private def transitionNoPathTables(frame: Frame, np: Datalog.NoPath): Table[Value] = {
    val nodeKey = NodeTypeKey(transType(np.ty))
    val linkKey = generateLinkKey(np.link)
    val key = NodeNotLinkedIndex.Key(nodeKey, linkKey, np.termIsSource)
    val bodyTable = np.t match {
      case Datalog.Var(name) =>
        if (frame.bodyTable.isBound(name))
          transitionUnaryIndexTable(frame.bodyTable, name, key)
        else
          throw IllegalDebugStateException("Cannot debug NoPath atom where the given variable is unbound")
      case Datalog.Constant(lit) =>
        throw IllegalDebugStateException("Cannot debug NoPath atom where the given term is a constant")
    }
    bodyTable
  }

  private def transitionUndefTables(frame: Frame, un: Datalog.Undef): Table[Value] = {
    val bodyTable = un.t match {
      case Datalog.Var(name) =>
        if (frame.bodyTable.isBound(name))
          Table.empty[Value](frame.bodyTable.columns)
        else
          frame.bodyTable
      case Datalog.Constant(_) =>
        Table.empty[Value](frame.bodyTable.columns)
    }
    bodyTable
  }

  private def transitionExtCallTables(frame: Frame, ext: Datalog.ExtensionalCall): Table[Value] = {
    val key = NamedRelationKey(ext.name, ext.args.size)

    val gensym = new Gensym(Set())
    val selectedIndices = ext.args.zipWithIndex.flatMap {
      case (Datalog.Var(name), idx) =>
        gensym.register(name)
        if (frame.bodyTable.isBound(name)) Some(idx)
        else None
      case (Datalog.Constant(_), idx) => Some(idx)
    }
    val mask = TupleMask.fromSelectedIndices(ext.args.size, selectedIndices.toArray)

    var argsTable: Table[Value] = Table.unit
    ext.args.foreach {
      case Datalog.Var(name) =>
        argsTable = argsTable.join(frame.bodyTable.project(Seq(name)))
      case Datalog.Constant(l) =>
        val newCol = gensym.fresh("const")
        argsTable.bind(newCol, transLiteral(l))
    }

    val extCallRows = argsTable.rows.flatMap { row =>
      val seed = Tuples.flatTupleOf(row.map(_.unwrap))
      database.enumerateTuples(key, mask, seed).asScala.map { tuple =>
        tuple.getElements.toSeq.map(Value.apply)
      }.toSeq
    }.toSeq
    val extCallColumns = ext.args.map {
      case Datalog.Var(name) => name
      case Datalog.Constant(_) => gensym.fresh("const")
    }
    val extCallTable = Table(extCallColumns, extCallRows)

    val extVarArgs = ext.args.collect { case Datalog.Var(name) => name}
    frame.bodyTable.join(extCallTable.project(extVarArgs))
  }

  private def transitionEqCompTables(frame: Frame, comp: Datalog.Compare): Table[Value] = {
    val bodyTable = frame.bodyTable
    val nextBodyTable: Table[Value] = (comp.lhs, comp.rhs) match {
      case (Datalog.Var(name1), Datalog.Var(name2)) =>
        if (bodyTable.isBound(name1) && bodyTable.isBound(name2))
          transitionEqCompBothBound(bodyTable, name1, name2)
        else if (bodyTable.isBound(name1) && !bodyTable.isBound(name2))
          transitionEqCompOneBound(bodyTable, name1, name2)
        else if (!bodyTable.isBound(name1) && bodyTable.isBound(name2))
          transitionEqCompOneBound(bodyTable, name2, name1)
        else
          throw IllegalDebugStateException("Cannot debug eq comparator where both arguments are not bound")
    case (Datalog.Var(name), Datalog.Constant(l)) =>
        if (bodyTable.isBound(name))
          transitionEqCompConstBound(bodyTable, name, transLiteral(l))
        else
          transitionEqCompConstUnbound(bodyTable, name, transLiteral(l))
      case (Datalog.Constant(l), Datalog.Var(name)) =>
        if (bodyTable.isBound(name))
          transitionEqCompConstBound(bodyTable, name, transLiteral(l))
        else
          transitionEqCompConstUnbound(bodyTable, name, transLiteral(l))
      case (Datalog.Constant(l1), Datalog.Constant(l2)) =>
        val v1 = transLiteral(l1)
        val v2 = transLiteral(l2)
        if (v1 == v2) frame.bodyTable
        else Table.empty(frame.bodyTable.columns)
    }
    nextBodyTable
  }

  private def transitionEqCompBothBound(table: Table[Value], col1: String, col2: String): Table[Value] = {
    val col1Index = table.columnIndex(col1)
    val col2Index = table.columnIndex(col2)
    table.filter { row =>
      row(col1Index) == row(col2Index)
    }
  }

  private def transitionEqCompOneBound(table: Table[Value], boundCol: String, unboundCol: String): Table[Value] = {
    val colIndex = table.columnIndex(boundCol)
    table.expand(unboundCol, row => row(colIndex))
  }

  private def transitionEqCompConstBound(table: Table[Value], col: String, v: Value): Table[Value] = {
    val colIdx = table.columnIndex(col)
    table.filter { row =>
      row(colIdx) == v
    }
  }

  private def transitionEqCompConstUnbound(table: Table[Value], col: String, v: Value): Table[Value] = {
    table.bind(col, v)
  }

  private def transitionNeqCompTables(frame: Frame, comp: Datalog.Compare): Table[Value] = {
    val bodyTable = frame.bodyTable
    val nextBodyTable: Table[Value] = (comp.lhs, comp.rhs) match {
      case (Datalog.Var(name1), Datalog.Var(name2)) =>
        if (bodyTable.isBound(name1) && bodyTable.isBound(name2))
          transitionNeqCompBothBound(bodyTable, name1, name2)
        else
          throw IllegalDebugStateException("Cannot debug neq comparator where one argument is not bound")
      case (Datalog.Var(name), Datalog.Constant(l)) =>
        if (bodyTable.isBound(name))
          transitionNeqCompOneConstant(bodyTable, name, transLiteral(l))
        else
          throw IllegalDebugStateException("Cannot debug neq comparator where one argument is not bound")
      case (Datalog.Constant(l), Datalog.Var(name)) =>
        if (bodyTable.isBound(name))
          transitionNeqCompOneConstant(bodyTable, name, transLiteral(l))
        else
          throw IllegalDebugStateException("Cannot debug neq comparator where one argument is not bound")
      case (Datalog.Constant(l1), Datalog.Constant(l2)) =>
        val v1 = transLiteral(l1)
        val v2 = transLiteral(l2)
        if (v1 == v2) frame.bodyTable
        else Table.empty(bodyTable.columns)
    }
    nextBodyTable
  }

  private def transitionNeqCompBothBound(table: Table[Value], col1: String, col2: String): Table[Value] = {
    val col1Idx = table.columnIndex(col1)
    val col2Idx = table.columnIndex(col2)
    table.filter { row =>
      row(col1Idx) != row(col2Idx)
    }
  }

  private def transitionNeqCompOneConstant(table: Table[Value], col: String, v: Value): Table[Value] = {
    val colIdx = table.columnIndex(col)
    table.filter { row =>
      row(colIdx) != v
    }
  }

  private def transitionReturnCallTables(callerFrame: Frame, calleeFrame: Frame): Frame.Tables = {
    val pat = calleeFrame.cp.point.pat
    val params = pat.params.map(_.name)
    val patternTable = fixpointState.relation(pat.name, calleeFrame.argsTable)
    transitionReturnCallTables(callerFrame, params, patternTable)
  }

  protected def transitionReturnCallTables(callerFrame: Frame, params: Seq[String], patternTable: Table[Value]): Frame.Tables = {
    // join bodyTable of caller with pattern table of callee
    val (_, args) = callerFrame.cp.atom.asCall.get
    val callArgVars = args.collect { case Datalog.Var(name) => name }
    val columnsSubst = params.zip(callArgVars).toMap
    val renamedPatternTable = patternTable.renameColumns(columnsSubst)
    val bodyTable = callerFrame.bodyTable.join(renamedPatternTable)

    (callerFrame.argsTable, bodyTable)
  }

  private def transitionReturnNegCallTables(callerFrame: Frame, calleFrame: Frame): Frame.Tables = {
    val pat = calleFrame.cp.point.pat
    val params = pat.params.map(_.name)
    val patternTable = fixpointState.relation(pat.name, calleFrame.argsTable)
    transitionReturnNegCallTables(callerFrame, params, patternTable)
  }

  private def transitionReturnNegCallTables(callerFrame: Frame, params: Seq[String], patternTable: Table[Value]): Frame.Tables = {
    // remove rows of caller bodyTable of that contains tuples of pattern table of callee
    val (_, args) = callerFrame.cp.atom.asCall.get
    val callArgVars = args.collect { case Datalog.Var(name) => name }
    val columnsSubst = params.zip(callArgVars).toMap
    val renamedPatternTable = patternTable.renameColumns(columnsSubst)
    val bodyTable = callerFrame.bodyTable.filter { row =>
      val columnValuePairs = callerFrame.bodyTable.columns.zip(row)
      !renamedPatternTable.contains(columnValuePairs)
    }
    (callerFrame.argsTable, bodyTable)
  }

  private def transitionCountAggTables(callerFrame: Frame, patternTable: Table[Value], lhs: Datalog.Term): Frame.Tables = {
    val count = patternTable.numRows
    transitionAggTables(callerFrame, lhs, ScalaValue(count))
  }

  private def transitionCustomAggTables(callerFrame: Frame, patternTable: Table[Value], lhs: Datalog.Term, agg: CustomAggregation) : Frame.Tables = {
    val valsToAgg = patternTable.rows.map {
      row => row(agg.aggregatedColumn).asScala
    }.toSeq
    val (initTerm, joinOpTerm) = getInitValueAndJoin(agg.agg)
    val initValue = executeScala(initTerm)
    val foldRes = valsToAgg.foldLeft(initValue.v) { case (res, x) =>
      // currently we assume that the previous result is the left op and the current value the right op
      val joinCode = s"(${joinOpTerm.syntax})($res, $x)"
      executeScala(joinCode).v
    }
    transitionAggTables(callerFrame, lhs, ScalaValue(foldRes))
  }


  private def getInitValueAndJoin(agg: Scala[meta.Term]): (Scala[meta.Term], Scala[meta.Term]) = {
    import scala.meta._
    var init: Option[meta.Term] = None
    var joinOp: Option[meta.Term] = None
    agg.tree match {
      case Term.NewAnonymous(Template(_, _, _, stats)) =>
        stats.foreach {
          case Defn.Def(_, Term.Name("init"), _, _, _, body) =>
            init = Some(body)
          case Defn.Def(_, Term.Name("join"), _, Seq(params), _, body) =>
            joinOp = Some(q"(..$params) => $body")
          case _ => // do nothing
        }
      case _ => throw IllegalDebugStateException("Object created for aggregation is not of type Aggregation")
    }
    if (init.isEmpty || joinOp.isEmpty) {
      throw IllegalDebugStateException("Aggregation has no initial value or join operation defined")
    }
    (Scala(init.get), Scala(joinOp.get))
  }

  private def transitionAggTables(callerFrame: Frame, lhs: Datalog.Term, v: ScalaValue): Frame.Tables = {
    val table = callerFrame.bodyTable
    val extBodyTable = lhs match {
      case Datalog.Var(name) =>
        if (table.isBound(name)) {
          val nameIdx = table.columnIndex(name)
          table.filter { row =>
            row(nameIdx) == v
          }
        } else
          table.bind(name, v)
      case Datalog.Constant(lit) =>
        val transLit = transLiteral(lit)
        if (transLit == v)
          table
        else
          Table.empty[Value](table.columns)
    }
    (callerFrame.argsTable, extBodyTable)
  }

  private def transLiteral(c: Datalog.Literal): Value = c match {
    case Datalog.IntLiteral(v) => ScalaValue(v)
    case Datalog.LongLiteral(v) => ScalaValue(v)
    case Datalog.DoubleLiteral(v) => ScalaValue(v)
    case Datalog.StringLiteral(v) => ScalaValue(v)
    case Datalog.BooleanLiteral(v) => ScalaValue(v)
  }

  private def transitionEvalTables(frame: Frame, lhs: Datalog.Term, eval: Datalog.Evaluation): Table[Value] = {
    val bodyTable = frame.bodyTable

    val lhsValue: Seq[Value] => Value = lhs match {
      case Datalog.Var(name) =>
        if (bodyTable.isBound(name)) {
          val cix = bodyTable.columnIndex(name)
          row => row(cix)
        } else {
          _ => throw new IllegalArgumentException
        }
      case Datalog.Constant(lit) =>
        val v = transLiteral(lit)
        _ => v
    }

    lhs match {
      case Datalog.Var(name) if !bodyTable.isBound(name) =>
        bodyTable.expand(Seq(name), { row =>
          Seq(executeScala(bodyTable, row, eval))
        })
      case _ =>
        bodyTable.filter { row =>
          val scalaValue = executeScala(bodyTable, row, eval)
          val lhsVal = lhsValue(row)
          scalaValue == lhsVal
        }
    }
  }

  private def executeScala(table: Table[Value], row: Seq[Value], eval: Datalog.Evaluation): ScalaValue = {
    val argTerms = eval.evalArgs.map {
      case (Datalog.Var(v), ty) => s"""$$env("$v").asInstanceOf[${ty.asScala.syntax}]"""
      case (Datalog.Constant(lit), _) => lit match {
        case Datalog.IntLiteral(v) => v.toString
        case Datalog.LongLiteral(v) => v.toString
        case Datalog.DoubleLiteral(v) => v.toString
        case Datalog.StringLiteral(v) => v.toString
        case Datalog.BooleanLiteral(v) => v.toString
      }
    }
    val argsMap: Map[String, Any] = eval.evalArgs.flatMap {
      case (Datalog.Var(v), _) => Some(v -> row(table.columnIndex(v)).unwrap)
      case (Datalog.Constant(_), _) => None
    }.toMap

    val funCode =
      s"""{ ($$env: Map[String, Any]) =>
         |  import ${defintionObjSym}.${compiled.name}._
         |  (${eval.code.syntax})(${argTerms.mkString(", ")})
         |}""".stripMargin

    val fun: Map[String, Any] => Any = scalaCompiler.compileAndLoadScala(funCode)
    ScalaValue(fun(argsMap))
  }

  private def executeScala(term: Scala[meta.Term]): ScalaValue =
    executeScala(term.syntax)

  private def executeScala(term: String): ScalaValue = {
    val code = s"import ${defintionObjSym}.${compiled.name}._\n$term"
    ScalaValue(scalaCompiler.compileAndLoadScala(code))
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
    val calledPat = patterns(name)
    val argsTable = prepareArgTableOfCall(frame, calledPat, args)
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
            transitionReturnNegCallTables(frame, params, calledPatTable)
          } else {
            transitionReturnCallTables(frame, params, calledPatTable)
          }
        val next = frame.cp.stepOver.get
        callStack.update(Frame(next, tables))
      case Datalog.Computed(lhs, _: CountAggregation) =>
        val tables = transitionCountAggTables(frame, calledPatTable, lhs)
        val next = frame.cp.stepOver.get
        callStack.update(Frame(next, tables))
      case Datalog.Computed(lhs, agg: CustomAggregation) =>
        val tables = transitionCustomAggTables(frame, calledPatTable, lhs, agg)
        val next = frame.cp.stepOver.get
        callStack.update(Frame(next, tables))
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

  def readDatabase(name: String, bindings: Table[Value]): Table[Value] = {
    val mainSpec = compiled.psystemModule.patterns.get(name) match {
      case Some(spec) => spec()
      case None => return Table.empty
    }
    val mainMatcher = engine.getMatcher(mainSpec)
    val unboundCols = patterns(name).params.map(_.name).diff(bindings.columns)
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
}