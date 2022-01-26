package inca.debugger

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
import inca.util.Scala
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
import org.eclipse.viatra.query.runtime.matchers.tuple.{TupleMask, Tuples}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truechange.{EditScript, URI}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

trait Debugger {
  type FrontendPoint
  def frontendPoint(cp: ControlPoint): Option[FrontendPoint]

  type FrontendValue
  def frontendTable(fp: FrontendPoint, bound: Table[Value]): Table[FrontendValue]

  // Datalog program information
  private var compiled: CompiledModule = _
  protected lazy val patterns: Map[String, Datalog.Pattern] =
    compiled.ir.pats.map { pat => pat.name -> pat }.toMap

  // Extensional database stuff
  protected var database: Database = _
  private var engine: AdvancedViatraQueryEngine =  _

  // Debugger state
  private val fixpointState: FixpointState = new FixpointState
  protected val callStack: CallStack = new CallStack()
  private val _controlTrace: ListBuffer[ControlPoint] = ListBuffer.empty
  protected val _controlTraceFrontend: ListBuffer[FrontendPoint] = ListBuffer.empty

  // Needed to execute scala code via reflection
  protected val scalaCompiler = new Scala.ScalaCompiler()
  protected var defintionObjSym: String = _

  // Accessor methods of debugger statej
  def frame: Frame = callStack.top

  def varsIR: Table[Value] = controlPointIR.point.bodies match {
    case BeforeList => frame.argsTable
    case AtListElem(_, _, _) => frame.bodyTable
    case AfterList => frame.patternTable
  }
  def varsFrontEnd: Table[FrontendValue] = frontendTable(controlPointFrontend, varsIR)

  def controlPointIR: ControlPoint = callStack.top.cp
  def controlPointFrontend: FrontendPoint = frontendPoint(controlPointIR).get

  def controlTraceIR: Seq[ControlPoint] = _controlTrace.toSeq
  def controlTraceFrontend: Seq[FrontendPoint] = _controlTraceFrontend.toSeq

  private def traceControlPoint(cp: ControlPoint): Unit = {
    _controlTrace += cp
    frontendPoint(cp).foreach(_controlTraceFrontend += _)
  }

  def relation(name: String): Table[Value] = fixpointState.relation(name)
  def relation(name: String, args: Table[Value]): Table[Value] =
    fixpointState.relation(name, args).getOrElse(throw IllegalDebugStateException(s"Pattern $name was never called with argument $args"))
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
  protected def entry(name: Datalog.Name, bindings: Table[Value]): Unit = {
    val pat = patterns(name)
    val cp = ControlPoint.patternEntryPoint(pat)
    val frame = Frame(cp, bindings, Table.empty, Table(pat.params.map(_.name), Seq()))
    callStack.push(frame)
    traceControlPoint(cp)
  }

  def stepIntoFrontend(): Unit

  def untilFinished(run: () => Unit): Unit =
    while (callStack.nonEmpty)
      stepIntoFrontend()

  def stepIntoUntil(stop: () => Boolean): Unit =
    while (!stop())
      stepInto()

  def stepInto(): Unit = {
    val frame = callStack.top
    frame.cp.point.atom match {
      case Some(atom) =>
        stepIntoNextAtom(frame, atom)
      case None =>
        stepIntoPatternBoundary(frame)
    }
    if (callStack.nonEmpty)
      traceControlPoint(callStack.top.cp)
  }

  def abortIfBodyFailed(): Unit =
    if (callStack.nonEmpty) {
      val next = callStack.top
      if (next.bodyTable.isEmpty) {
        val pattern = next.cp.point.pat
        // the empty body table has to range over all pattern parameters
        val emptyBodyTable = Table.empty[Value](pattern.params.map(_.name))
        callStack.update(next.copy(cp = next.cp.abortBody, bodyTable = emptyBodyTable))
      }
    }

  def stepIntoNextAtom(frame: Frame, atom: Datalog.Atom): Unit = {
    if (atom.asCall.isDefined) {
      stepIntoCall(frame, atom)
    } else {
      val nextBodyTable = transitionAtomTables(frame, atom)
      val next = frame.cp.stepIntra.get // yields next atom
      callStack.update(frame.copy(cp = next, bodyTable = nextBodyTable))
    }
    abortIfBodyFailed()
  }

  def stepIntoCall(frame: Frame, atom: Datalog.Atom): Unit = atom match {
    case Datalog.Call(name, args, _, neg) =>
      val pattern = patterns(name)
      val preTables = prepareCallTables(frame, pattern, args)
      // if we called the pattern already before with the same argument lookup table
      // else step into pattern
      if (fixpointState.contains(name, preTables._1)) {
        val tables = readPatternTable(frame, name, args, neg)
        val next = frame.cp.stepOver.get
        callStack.update(Frame(next, tables))
      } else {
        val callee = ControlPoint(PatternPoint(pattern, BeforeList))
        if (neg) {
          checkNegativeCallArguments(args, frame.bodyTable)
        }
        callStack.push(Frame(callee, preTables))
      }
    case Datalog.Computed(lhs, agg: Datalog.CountAggregation) =>
      val pattern = patterns(agg.patName)
      val preTables = prepareCallTables(frame, pattern, agg.args)
      if (fixpointState.contains(agg.patName, preTables._1)) {
        val (_, _, patternTable) = readPatternTable(frame, agg.patName, agg.args)
        val next = frame.cp.stepIntra.getOrElse(throw IllegalDebugStateException("Cannot have non-atom frame below pattern end frame on call stack"))
        val tables = transitionCountAggTables(frame, patternTable, lhs)
        callStack.update(Frame(next, tables))
      } else {
        val callee = ControlPoint(PatternPoint(patterns(agg.patName), BeforeList))
        callStack.push(Frame(callee, preTables))
      }
    case Datalog.Computed(lhs, agg: Datalog.CustomAggregation) =>
      val pattern = patterns(agg.patName)
      val preTables = prepareCallTables(frame, pattern, agg.args)
      if (fixpointState.contains(agg.patName, preTables._1)) {
        val (_, _, patternTable) = readPatternTable(frame, agg.patName, agg.args)
        val next = frame.cp.stepIntra.getOrElse(throw IllegalDebugStateException("Cannot have non-atom frame below pattern end frame on call stack"))
        val tables = transitionCountAggTables(frame, patternTable, lhs)
        callStack.update(Frame(next, tables))
      } else {
        val callee = ControlPoint(PatternPoint(pattern, BeforeList))
        callStack.push(Frame(callee, preTables))
      }
    case _ => throw IllegalDebugStateException(s"Atom $atom should contain call")
  }

  private def readPatternTable(callerFrame: Frame, name: String, args: Seq[Datalog.Term], neg: Boolean = false): Frame.Tables = {
    val patternTable = fixpointState.relation(name)
    val params = patterns(name).params.map(_.name)
    if (neg)
      transitionReturnNegCallTables(frame, params, patternTable)
    else {
      transitionReturnCallTables(frame, params, patternTable)
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
    callStack.update(Frame(next,  frame.argsTable, frame.argsTable, frame.patternTable))
  }

  protected def doPatternExit(frame: Frame): Unit = {
    val pat = frame.cp.point.pat
    callStack.pop() // pop pattern exit point

    // extend derived relations
    fixpointState.add(pat.name, frame.argsTable, frame.patternTable)

    // if the stack is still not empty there should be a call, or an aggregation on top
    if (callStack.nonEmpty) {
      val callerFrame = callStack.top
      callerFrame.cp.atom match {
        case Datalog.Call(_, _, _, false) =>
          val next = callerFrame.cp.stepIntra
            .getOrElse(throw IllegalDebugStateException("Cannot have non-atom frame below pattern end frame on call stack"))
          val tables = transitionReturnCallTables(callerFrame, frame)
          callStack.update(Frame(next, tables))
        case Datalog.Call(_, _, _, true) =>
          val next = callerFrame.cp.stepIntra
            .getOrElse(throw IllegalDebugStateException("Cannot have non-atom frame below pattern end frame on call stack"))
          val tables = transitionReturnNegCallTables(callerFrame, frame)
          callStack.update(Frame(next, tables))
        case Datalog.Computed(lhs, custAgg: CustomAggregation) =>
          val next = callerFrame.cp.stepIntra
            .getOrElse(throw IllegalDebugStateException("Cannot have non-atom frame below pattern end frame on call stack"))
          val tables = transitionCustomAggTables(callerFrame, frame.patternTable, lhs, custAgg)
          callStack.update(Frame(next, tables))
        case Datalog.Computed(lhs, countAgg: CountAggregation) =>
          val next = callerFrame.cp.stepIntra
            .getOrElse(throw IllegalDebugStateException("Cannot have non-atom frame below pattern end frame on call stack"))
          val tables = transitionCountAggTables(callerFrame, frame.patternTable, lhs)
          callStack.update(Frame(next, tables))
      }
    }
  }

  protected def doBodyEntry(frame: Frame, cp: ControlPoint): Unit = {
    val next = cp.stepIntra.get // yields first atom of this body
    callStack.update(Frame(next, frame.argsTable, frame.argsTable, frame.patternTable))
  }

  protected def doBodyExit(frame: Frame, cp: ControlPoint): Unit = {
    val next = cp.stepIntra.get // yields entry of next body
    val pattern = frame.cp.point.pat
    val params = pattern.params.map(_.name)
    val projectedBodyTable = frame.bodyTable.project(params)
    fixpointState.add(pattern.name, frame.argsTable, projectedBodyTable)
    val tables = transitionNextBodyTables(frame)
    callStack.update(Frame(next, tables))
  }

  private def stepIntoPatternBoundary(frame: Frame): Unit = {
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
  protected def prepareCallTables(frame: Frame, calledPattern: Datalog.Pattern, args: Seq[Datalog.Term]): Frame.Tables = {
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

    val patternTable = Table.empty[Value](params)

    (argsTable, argsTable, patternTable)
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
        val uri = row(colIdx).asURI
        database.containsTuple(key, Tuples.staticArityFlatTupleOf(uri))
      }
    } else {
      val uriRows = database.enumerateValues(key, TupleMask.empty(0), Tuples.staticArityFlatTupleOf()).iterator().asScala.map { case uri: URI =>
        Seq(URIValue(uri))
      }.toSeq
      val nameTable = Table[Value](Seq(col), uriRows)
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
    val srcIdx = table.columnIndex(src)
    val trgIdx = table.columnIndex(trg)
    table.filter { row =>
      val srcURI = row(srcIdx)
      val trgURI = row(trgIdx)
      database.containsTuple(key, Tuples.staticArityFlatTupleOf(srcURI, trgURI))
    }
  }

  private def transitionBinaryIndexQueryOneBound(table: Table[Value], key: IInputKey, bound: String, unbound: String, isSourceBound: Boolean): Table[Value] = {
    val selectIdx = if (isSourceBound) 0 else 1
    val mask = TupleMask.selectSingle(selectIdx, 2)
    val boundIdx = table.columnIndex(bound)
    table.expand(unbound, { row =>
      val boundURI = row(boundIdx).asURI
      val unboundURI = database.enumerateValues(key, mask, Tuples.staticArityFlatTupleOf(boundURI)).iterator().next()
      convertDatabaseTupleValue(unboundURI)
    })
  }

  private def transitionBinaryIndexQueryUnbound(table: Table[Value], key: IndexKey[_], src: String, trg: String): Table[Value] = {
    val rows = database.enumerateTuples(key, TupleMask.empty(2), Tuples.staticArityFlatTupleOf()).iterator().asScala.map { tuple =>
      val src = tuple.get(0)
      val trg = tuple.get(1)
      Seq(convertDatabaseTupleValue(src), convertDatabaseTupleValue(trg))
    }
    val srcTrgTable = Table(Seq(src, trg), rows.toSeq)
    table.join(srcTrgTable)
  }

  private def convertDatabaseTupleValue(value: Any): Value = value match {
    case uri: URI => URIValue(uri)
    case v: Any => ScalaValue(v)
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
    // TODO implement this correctly
    Table.empty
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
    val params = calleeFrame.cp.point.pat.params.map(_.name)
    transitionReturnCallTables(callerFrame, params, calleeFrame.patternTable)
  }

  protected def transitionReturnCallTables(callerFrame: Frame, params: Seq[String], patternTable: Table[Value]): Frame.Tables = {
    // join bodyTable of caller with pattern table of callee
    val (_, args) = callerFrame.cp.atom.asCall.get
    val callArgVars = args.collect { case Datalog.Var(name) => name }
    val columnsSubst = params.zip(callArgVars).toMap
    val renamedPatternTable = patternTable.renameColumns(columnsSubst)
    val bodyTable = callerFrame.bodyTable.join(renamedPatternTable)

    (callerFrame.argsTable, bodyTable, callerFrame.patternTable)
  }

  private def transitionReturnNegCallTables(callerFrame: Frame, calleFrame: Frame): Frame.Tables = {
    val params = calleFrame.cp.point.pat.params.map(_.name)
    transitionReturnNegCallTables(callerFrame, params, calleFrame.patternTable)
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

    (callerFrame.argsTable, bodyTable, callerFrame.patternTable)
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
    (callerFrame.argsTable, extBodyTable, callerFrame.patternTable)
  }


  private def transitionNextBodyTables(frame: Frame): Frame.Tables = {
    // extend pattern table with tuples derived by body
    val pat = frame.cp.point.pat
    val columns = pat.params.map(_.name).filter(frame.bodyTable.columns.contains)
    val projectedBodySubst = frame.bodyTable.project(columns)
    val patternTable = frame.patternTable.addRows(projectedBodySubst)

    (frame.argsTable, frame.argsTable, patternTable)
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
      case (Datalog.Var(v), _) => Some(v -> row(table.columnIndex(v)).inner)
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

  def stepOverUntil(stop: () => Boolean): Unit =
    while (!stop())
      stepOver()

  def stepOver(): Unit = {
    val frame = callStack.top
    frame.cp.point.atom match {
      case Some(atom) =>
        atom match {
          case Datalog.Call(name, args, _, false) =>
            val pattern = patterns(name)
            val (argsTable, _, _) = prepareCallTables(frame, pattern, args)
            val callPatternTable = readDatabase(name, argsTable)
            val params = pattern.params.map(_.name)
            val tables = transitionReturnCallTables(frame, params, callPatternTable)
            val next = frame.cp.stepOver.get
            callStack.update(Frame(next, tables))

          case Datalog.Call(name, args, _, true) =>
            val pattern = patterns(name)
            val (argsTable, _, _) = prepareCallTables(frame, pattern, args)
            checkNegativeCallArguments(args, frame.bodyTable)
            val callPatternTable = readDatabase(name, argsTable)
            val params = pattern.params.map(_.name)
            val tables = transitionReturnNegCallTables(frame, params, callPatternTable)
            val next = frame.cp.stepOver.get
            callStack.update(Frame(next, tables))

          case Datalog.Computed(lhs, countAgg: CountAggregation) =>
            val pattern = patterns(countAgg.patName)
            val (argsTable, _, _) = prepareCallTables(frame, pattern, countAgg.args)
            val callPatternTable = readDatabase(countAgg.patName, argsTable)
            val tables = transitionCountAggTables(frame, callPatternTable, lhs)
            val next = frame.cp.stepOver.get
            callStack.update(Frame(next, tables))

          case Datalog.Computed(lhs, customAgg: CustomAggregation) =>
            val pattern = patterns(customAgg.patName)
            val (argsTable, _, _) = prepareCallTables(frame, pattern, customAgg.args)
            val callPatternTable = readDatabase(customAgg.patName, argsTable)
            val tables = transitionCustomAggTables(frame, callPatternTable, lhs, customAgg)
            val next = frame.cp.stepOver.get
            callStack.update(Frame(next, tables))

          case _ =>
            stepIntoNextAtom(frame, atom)
        }
      case None =>
        if (frame.cp.isPatternPoint) {
          val pattern = frame.cp.point.pat
          val patternTable = readDatabase(pattern.name, frame.argsTable)
          val next = ControlPoint(PatternPoint(pattern, AfterList))
          callStack.update(Frame(next, frame.argsTable, patternTable, patternTable))
        } else if (frame.cp.isBodyPoint) {
          // we cannot read from the database because we dont know which tuples where derived by a specific body
          val next = frame.cp.stepOver.get
          // we run until the the next breakpoint
          // TODO is there a better way to do this?
          runUntil(next)
        } else {
          stepIntoPatternBoundary(frame)
        }
    }
    if (callStack.nonEmpty)
      traceControlPoint(callStack.top.cp)
  }

  private def runUntil(cp: ControlPoint): Unit = {
    val currentStackSize = callStack.size
    // we run stepInto until we reach the target controlpoint and the stack size is the same as it was before
    while (!(callStack.top.cp == cp && callStack.size == currentStackSize)) {
      stepInto()
    }
  }

  def readDatabase(name: String, bindings: Table[Value]): Table[Value] = {
    val mainSpec = compiled.psystemModule.patterns.get(name) match {
      case Some(spec) => spec()
      case None => return Table.empty
    }
    val mainMatcher = engine.getMatcher(mainSpec)
    val rows = bindings.rows.flatMap { row =>
      val unboundCols = patterns(name).params.map(_.name).diff(bindings.columns)
      val inputMap = bindings.columns.zip(row.map(_.inner)).toMap ++ unboundCols.map( _ -> null)
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