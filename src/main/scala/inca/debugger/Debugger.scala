package inca.debugger

import inca.backend.ir.Datalog
import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.Options
import inca.debugger.Frame.Tables
import inca.debugger.table.Table
import inca.runtime.context.{DataModel, QueryScope}
import inca.runtime.index.dynamic.ParentIndex
import inca.runtime.index.{IndexKey, LinkListNextKey, LinkNodeKey, LinkPrimitiveKey, NodeTypeKey}
import inca.runtime.index.virtual.{NodeNotLinkedIndex, NotNodeTypeIndex, SizeIndex}
import inca.runtime.EnginePool
import inca.runtime.db.Database
import inca.util.Scala
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
import org.eclipse.viatra.query.runtime.matchers.tuple.{TupleMask, Tuples}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truechange.{EditScript, URI}
import truediff.Diffable

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.meta.Term
import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

trait Debugger {
  val frontend: DebuggerFrontend

  // Datalog program information
  private var module: Datalog.Module = _
  private var dataModel: DataModel = _
  implicit lazy val patterns: Map[String, Datalog.Pattern] = {
    module.pats.map { pat => pat.name -> pat }.toMap
  }

  // Extensional database stuff
  private var database: Database = _
  private var engine: AdvancedViatraQueryEngine =  _

  // Debugger state
  private var fixpointState: FixpointState = FixpointState(Map())
  private val callStack: CallStack = new CallStack()
  private val _controlTrace: ListBuffer[ControlPoint] = ListBuffer.empty

  // Needed to execute scala code via reflection
  private val toolBox: ToolBox[universe.type] =
    universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

  // Accessor methods of debugger statej
  def controlTrace: Seq[ControlPoint] = _controlTrace.toSeq
  def relation(name: String): Table = fixpointState.derived(name)
  def isFinished: Boolean = callStack.isFinished

  // initialization methods
  def initialize(mod: Datalog.Module, dm: DataModel, tree: Diffable): Unit =
    initialize(mod, dm, Diffable.load(tree))

  def initialize(mod: Datalog.Module, dm: DataModel, edits: EditScript): Unit = {
    module = mod
    dataModel = dm
    val (_engine, _database) = compileModule()
    engine = _engine
    database = _database
    _database.processEditScript(edits)
  }

  private def compileModule(): (AdvancedViatraQueryEngine, Database) = {
    val options = Options(_stopOnError = true, _stopOnWarning = false)

    val compiled = inca.compiler.Compiler.compileGP(module, dataModel, options)
    val scope = new QueryScope(compiled.dataModel)
    EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
  }

  // Debugger methods
  def entry(name: Datalog.Name, bindings: Table): Unit = {
    val pat = patterns(name)
    val cp = ControlPoint.patternEntryPoint(pat)
    val frame = Frame(cp, bindings, Table.empty, Table(pat.params.map(_.name), Seq()))
    callStack.push(frame)
    _controlTrace += cp
  }

  def stepInto(): Unit = {
    val frame = callStack.top
    val cp = frame.cp
    cp.point.atom match {
      case Some(call: Datalog.Call) =>
        val tables = prepareCallTables(frame, call)
        val callee = ControlPoint(PatternPoint(patterns(call.name), BeforeList))
        if (call.neg) {
          call.args.foreach {
            case Datalog.Var(name) if !frame.bodyTable.isBound(name) =>
              throw IllegalDebugStateException(s"All arguments of a negative pattern call have to be bound, but $name is not bound")
            case _ => // do nothing
          }
        }
        callStack.push(Frame(callee, tables))
      case Some(comp@Datalog.Computed(_, countAgg: Datalog.CountAggregation)) =>
        val tables = prepareAggregationCallTables(frame, countAgg)
        val callee = ControlPoint(PatternPoint(patterns(countAgg.patName), BeforeList))
        callStack.push(Frame(callee, tables))
      case Some(comp@Datalog.Computed(_, custAgg: Datalog.CustomAggregation)) =>
        val tables = prepareAggregationCallTables(frame, custAgg)
        val callee = ControlPoint(PatternPoint(patterns(custAgg.patName), BeforeList))
        callStack.push(Frame(callee, tables))
      case Some(atom) =>
        val tables = transitionAtomTables(frame, atom)
        val next = cp.stepIntra.get // yields next atom
        callStack.update(Frame(next, tables))
      case None =>
        if (cp.point.isPatternEntry) {
          val next = cp.stepIntra.get // yields first body of this pattern
          callStack.update(Frame(next,  frame.argsTable, frame.argsTable, frame.patternTable))
        } else if (cp.point.isPatternExit) {
          val pat = frame.cp.point.pat
          callStack.pop() // pop pattern exit point

          // extend derived relations
          fixpointState = fixpointState.extendRelation(pat.name, frame.patternTable)

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
              case Datalog.Computed(lhs, custAgg: Datalog.CustomAggregation) =>
                // TODO we need to aggregate over the results of the pattern
              case Datalog.Computed(lhs, countAgg: Datalog.CountAggregation) =>
                // TODO we need to count the number of results of the pattern
            }
          }
        } else if (cp.point.isBodyEntry) {
          val next = cp.stepIntra.get // yields first atom of this body
          callStack.update(Frame(next, frame.argsTable, frame.argsTable, frame.patternTable))
        } else if (cp.point.isBodyExit) {
          val next = cp.stepIntra.get // yields entry of next body
          val tables = transitionNextBodyTables(frame)
          callStack.update(Frame(next, tables))
        } else {
          throw new IllegalStateException(s"Unexpected control point $cp")
        }
    }
  }

  // Methods to prepare frame tables for atoms that can jump into another pattern (calls and aggregations)
  private def prepareCallTables(frame: Frame, call: Datalog.Call): Frame.Tables = {
    val callingPat = patterns(call.name)
    val params = callingPat.params.map(_.name)

    // prepare argsTable
    val paramSubst = params.zip(call.args)
    val (varsBindings, constBindings) = paramSubst.partition(_._2.isInstanceOf[Datalog.Var])
    val varsBindingsCast = varsBindings.map { case (p, v) => (p, v.asInstanceOf[Datalog.Var]) }
    val constBindingsCast = constBindings.map { case (p, v) => (p, v.asInstanceOf[Datalog.Constant]) }
    val columnsSubst = varsBindingsCast.map { case (p, v) => (v.name, p) }.toMap
    var argsTable = frame.bodyTable.project(varsBindingsCast.map(_._2.name)).renameColumns(columnsSubst)
    constBindingsCast.foreach { case (p, c) =>
      argsTable = argsTable.bind(p, transLiteral(c.lit))
    }

    val patternTable = Table.empty(params)

    (argsTable, argsTable, patternTable)
  }


  private def prepareAggregationCallTables(frame: Frame, agg: Datalog.Computation): Frame.Tables = agg match {
    case Datalog.CountAggregation(patName, args) =>
      // TODO implement
      ???
    case Datalog.CustomAggregation(typ, description, agg, patName, args, aggregatedColumn) =>
      // TODO implement
      ???
    case Datalog.Evaluation(evalArgs, resultType, code) =>
      throw IllegalDebugStateException(s"The function prepareAggregationCallTables should not be called with evaluation computation $agg")
  }


  // methods to prepare frame tables for atoms that do not jump into another pattern (atom is not a call, or aggregation)
  private def transitionAtomTables(frame: Frame, atom: Datalog.Atom): Frame.Tables = atom match {
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
    case comp@Datalog.Computed(_, eval: Datalog.Evaluation) =>
      transitionEvalTables(frame, comp)
    case Datalog.Call(_, _, _, _) =>
      throw IllegalDebugStateException(s"The function transitionAtomTables should not be called with call atom $atom")
    case Datalog.Computed(_, _) =>
      throw IllegalDebugStateException(s"The function transitionAtomTables should not be called with computed atom $atom")
  }


  // method to prepare frame tables of atoms that query unary edb relations (has type and not has type)
  private def transitionHasTypeTables(frame: Frame, ht: Datalog.HasType): Frame.Tables = {
    val bodyTable = ht.t match {
      case Datalog.Var(name) =>
        val key = NodeTypeKey(transType(ht.typ))
        transitionUnaryIndexTable(frame.bodyTable, name, key)
      case Datalog.Constant(_) =>
        throw new IllegalArgumentException(s"HasType is not defined on constants $ht")
    }
    (frame.argsTable, bodyTable, frame.patternTable)
  }

  private def transType(typ: Datalog.Type): truechange.Type = typ match {
    case Datalog.TAny => truechange.AnyType
    case Datalog.TNode(name) => truechange.SortType(name)
    case Datalog.TList(ty) => truechange.ListType(transType(ty))
    case _ => throw new IllegalArgumentException("NOT SUPPORTED YET")
  }

  private def transitionNotHasTypeTables(frame: Frame, nht: Datalog.NotHasType): Frame.Tables = {
    val bodyTable = nht.t match {
      case Datalog.Var(name) =>
        val key = NotNodeTypeIndex.Key(transType(nht.typ))
        transitionUnaryIndexTable(frame.bodyTable, name, key)
      case Datalog.Constant(_) =>
        throw new IllegalArgumentException(s"HasType is not defined on constants $nht")
    }
    (frame.argsTable, bodyTable, frame.patternTable)
  }

  private def transitionUnaryIndexTable(table: Table, col: String, key: IInputKey): Table = {
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
      val nameTable = Table(Seq(col), uriRows.toSeq)
      table.join(nameTable)
    }
  }


  // methods to prepare frame tables for atoms that query binary edb relations (path, nopath)
  private def transitionPathTables(frame: Frame, p: Datalog.Path): Frame.Tables = {
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
    (frame.argsTable, nextBodyTable, frame.patternTable)
  }

  private def transitionBinaryIndexQueryBothBound(table: Table, key: IInputKey, src: String, trg: String): Table = {
    val srcIdx = table.columnIndex(src)
    val trgIdx = table.columnIndex(trg)
    table.filter { row =>
      val srcURI = row(srcIdx)
      val trgURI = row(trgIdx)
      database.containsTuple(key, Tuples.staticArityFlatTupleOf(srcURI, trgURI))
    }
  }

  private def transitionBinaryIndexQueryOneBound(table: Table, key: IInputKey, bound: String, unbound: String, isSourceBound: Boolean): Table = {
    val extendedTable = table.addColumn(unbound)
    val selectIdx = if (isSourceBound) 0 else 1
    val mask = TupleMask.selectSingle(selectIdx, 2)
    val boundIdx = table.columnIndex(bound)
    extendedTable.map { row =>
      val boundURI = row(boundIdx).asURI
      val unboundURI = database.enumerateValues(key, mask, Tuples.staticArityFlatTupleOf(boundURI)).iterator().next()
      row :+ convertDatabaseTupleValue(unboundURI)
    }
  }

  private def transitionBinaryIndexQueryUnbound(table: Table, key: IndexKey[_], src: String, trg: String): Table = {
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
      if (dataModel.links.contains(link))
        LinkNodeKey(link)
      else
        LinkPrimitiveKey(link)
  }

  private def transitionNoPathTables(frame: Frame, np: Datalog.NoPath): Frame.Tables = {
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
    (frame.argsTable, bodyTable, frame.patternTable)
  }

  private def transitionUndefTables(frame: Frame, un: Datalog.Undef): Frame.Tables = {
    val bodyTable = un.t match {
      case Datalog.Var(name) =>
        if (frame.bodyTable.isBound(name))
          Table.empty(frame.bodyTable.columns)
        else
          frame.bodyTable
      case Datalog.Constant(_) =>
        Table.empty(frame.bodyTable.columns)
    }
    (frame.argsTable, bodyTable, frame.patternTable)
  }

  private def transitionExtCallTables(frame: Frame, ext: Datalog.ExtensionalCall): Frame.Tables = ???

  private def transitionEqCompTables(frame: Frame, comp: Datalog.Compare): Tables = {
    val bodyTable = frame.bodyTable
    val nextBodyTable = (comp.lhs, comp.rhs) match {
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
    (frame.argsTable, nextBodyTable, frame.patternTable)
  }

  private def transitionEqCompBothBound(table: Table, col1: String, col2: String): Table = {
    val col1Index = table.columnIndex(col1)
    val col2Index = table.columnIndex(col2)
    table.filter { row =>
      row(col1Index) == row(col2Index)
    }
  }

  private def transitionEqCompOneBound(table: Table, boundCol: String, unboundCol: String): Table = {
    val colIndex = table.columnIndex(boundCol)
    val extendedTable = table.addColumn(unboundCol)
    extendedTable.map { row =>
      row :+ row(colIndex)
    }
  }

  private def transitionEqCompConstBound(table: Table, col: String, v: Value): Table = {
    val colIdx = table.columnIndex(col)
    table.filter { row =>
      row(colIdx) == v
    }
  }

  private def transitionEqCompConstUnbound(table: Table, col: String, v: Value): Table = {
    table.bind(col, v)
  }

  private def transitionNeqCompTables(frame: Frame, comp: Datalog.Compare): Tables = {
    val bodyTable = frame.bodyTable
    val nextBodyTable = (comp.lhs, comp.rhs) match {
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
    (frame.argsTable, nextBodyTable, frame.patternTable)
  }

  private def transitionNeqCompBothBound(table: Table, col1: String, col2: String): Table = {
    val col1Idx = table.columnIndex(col1)
    val col2Idx = table.columnIndex(col2)
    table.filter { row =>
      row(col1Idx) != row(col2Idx)
    }
  }

  private def transitionNeqCompOneConstant(table: Table, col: String, v: Value): Table = {
    val colIdx = table.columnIndex(col)
    table.filter { row =>
      row(colIdx) != v
    }
  }

  private def transitionReturnCallTables(callerFrame: Frame, calleFrame: Frame): Frame.Tables = {
    // join bodyTable of caller with pattern table of callee
    val (_, args) = callerFrame.cp.atom.asCall.get
    val callArgVars = args.collect { case Datalog.Var(name) => name }
    val columnsSubst = calleFrame.cp.point.pat.params.map(_.name).zip(callArgVars).toMap
    val renamedPatternTable = calleFrame.patternTable.renameColumns(columnsSubst)
    val bodyTable = callerFrame.bodyTable.join(renamedPatternTable)

    (callerFrame.argsTable, bodyTable, callerFrame.patternTable)
  }

  private def transitionReturnNegCallTables(callerFrame: Frame, calleFrame: Frame): Frame.Tables = {
    // remove rows of caller bodyTable of that contains tuples of pattern table of callee
    val (_, args) = callerFrame.cp.atom.asCall.get
    val callArgVars = args.collect { case Datalog.Var(name) => name }
    val columnsSubst = calleFrame.cp.point.pat.params.map(_.name).zip(callArgVars).toMap
    val renamedPatternTable = calleFrame.patternTable.renameColumns(columnsSubst)
    val bodyTable = callerFrame.bodyTable.filter { row =>
      val columnValuePairs = callerFrame.bodyTable.columns.zip(row)
      !renamedPatternTable.contains(columnValuePairs)
    }

    (callerFrame.argsTable, bodyTable, callerFrame.patternTable)
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

  private def transitionEvalTables(frame: Frame, comp: Datalog.Computed): Frame.Tables = {
    val bodyTable = frame.bodyTable
    val Datalog.Evaluation(evalArgs, resultType, code) = comp.computation

    // prepare table for arguments of scala code
    val (evalVarArgs, evalConstArgs) = evalArgs.map(_._1).zip(code.tree.params).partitionMap {
      case (Datalog.Var(v), p) => Left((p.name, v))
      case (Datalog.Constant(l), p) => Right((p.name, transLiteral(l)))
    }
    var argsTable = bodyTable.project(evalVarArgs.map(_._2))
    evalConstArgs.foreach { case(p, v) =>
      argsTable = argsTable.bind(p.value, v)
    }
    argsTable = argsTable.rearrangeColumns(code.tree.params.map(_.name.value))

    // execute scala code for each row of table
    val results =
      if (argsTable.isEmpty)
        Seq(executeScala(Seq(), code))
      else argsTable.data.map { tuple =>
        executeScala(tuple, code)
      }

    // transition eq constraint for each result
    val multipleBodyTable = results.map { result =>
      comp.lhs match {
        case Datalog.Var(name) =>
          frame.bodyTable.bind(name, result)
        case Datalog.Constant(lit) =>
          if (transLiteral(lit) == result) {
            bodyTable
          } else
            Table.empty(bodyTable.columns)
      }
    }

    // merge resulting tables
    var extBodyTable = multipleBodyTable.head
    multipleBodyTable.tail.foreach { table =>
      extBodyTable.addRows(table)
    }

    (frame.argsTable, extBodyTable, frame.patternTable)
  }

  private def executeScala(tuple: Seq[Value], code: Scala[Term.Function]): ScalaValue = {
    val funCode = s"(${code.syntax})(${tuple.mkString(", ")})"
    val parsed = toolBox.parse(funCode)
    ScalaValue(toolBox.eval(parsed))
  }
}