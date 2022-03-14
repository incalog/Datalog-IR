package inca.debugger

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.CustomAggregation
import inca.compiler.CompiledModule
import inca.debugger.table.Table
import inca.runtime.db.Database
import inca.runtime.index.{IndexKey, LinkListNextKey, LinkNodeKey, LinkPrimitiveKey, NamedRelationKey, NodeTypeKey}
import inca.runtime.index.dynamic.ParentIndex
import inca.runtime.index.virtual.{NodeNotLinkedIndex, NotNodeTypeIndex, SizeIndex}
import inca.util.{Gensym, Scala}
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
import org.eclipse.viatra.query.runtime.matchers.tuple.{TupleMask, Tuples}

import scala.jdk.CollectionConverters._

class TableOps(var database: Database, val compiled: CompiledModule, val fixpointState: FixpointState[Value]) {

  // Needed to execute scala code via reflection
  private lazy val scalaCompiler = new Scala.ScalaCompiler()
  private lazy val defintionObjSym: String = scalaCompiler.define {
    import scala.meta._
    q"object O {..${compiled.psystemSource.stats}}".syntax
  }

  def getDefinitionObjSym: String = defintionObjSym
  def compileAndLoadScala[A](source: String): A =
    scalaCompiler.compileAndLoadScala(source)

  // Methods to prepare frame tables for atoms that can jump into another pattern (calls and aggregations)
  def prepareArgTableOfCall(frame: Frame, calledPattern: Datalog.Pattern, args: Seq[Datalog.Term]): Table[Value] = {
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
  def transitionAtomTables(frame: Frame, atom: Datalog.Atom): Table[Value] = atom match {
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
  def transitionHasTypeTables(frame: Frame, ht: Datalog.HasType): Table[Value] =
    ht.t match {
      case Datalog.Var(name) =>
        val key = NodeTypeKey(transType(ht.typ))
        transitionUnaryIndexTable(frame.bodyTable, name, key)
      case Datalog.Constant(_) =>
        throw new IllegalArgumentException(s"HasType is not defined on constants $ht")
    }

  def transType(typ: Datalog.Type): truechange.Type = typ match {
    case Datalog.TAny => truechange.AnyType
    case Datalog.TNode(name) => truechange.SortType(name)
    case Datalog.TList(ty) => truechange.ListType(transType(ty))
    case _ => throw new IllegalArgumentException("NOT SUPPORTED YET")
  }

  def transitionNotHasTypeTables(frame: Frame, nht: Datalog.NotHasType): Table[Value] =
    nht.t match {
      case Datalog.Var(name) =>
        val key = NotNodeTypeIndex.Key(transType(nht.typ))
        transitionUnaryIndexTable(frame.bodyTable, name, key)
      case Datalog.Constant(_) =>
        throw new IllegalArgumentException(s"HasType is not defined on constants $nht")
    }

  def transitionUnaryIndexTable(table: Table[Value], col: String, key: IInputKey): Table[Value] = {
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
  def transitionPathTables(frame: Frame, p: Datalog.Path): Table[Value] = {
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

  def transitionBinaryIndexQueryBothBound(table: Table[Value], key: IInputKey, src: String, trg: String): Table[Value] = {
    val idxL = table.columnIndex(src)
    val idxR = table.columnIndex(trg)
    table.filter { row =>
      val vL = row(idxL).unwrap
      val vR = row(idxR).unwrap
      database.containsTuple(key, Tuples.staticArityFlatTupleOf(vL, vR))
    }
  }

  def transitionBinaryIndexQueryOneBound(table: Table[Value], key: IInputKey, bound: String, unbound: String, isSourceBound: Boolean): Table[Value] = {
    val selectIdx = if (isSourceBound) 0 else 1
    val mask = TupleMask.selectSingle(selectIdx, 2)
    val boundIdx = table.columnIndex(bound)
    table.expand(unbound, { row =>
      val boundV = row(boundIdx).unwrap
      val unboundURI = database.enumerateValues(key, mask, Tuples.staticArityFlatTupleOf(boundV)).iterator().next()
      Value(unboundURI)
    })
  }

  def transitionBinaryIndexQueryUnbound(table: Table[Value], key: IndexKey[_], src: String, trg: String): Table[Value] = {
    val rows = database.enumerateTuples(key, TupleMask.empty(2), Tuples.staticArityFlatTupleOf()).iterator().asScala.map { tuple =>
      val vL = tuple.get(0)
      val vR = tuple.get(1)
      Seq(Value(vL), Value(vR))
    }
    val srcTrgTable = Table(Seq(src, trg), rows.toSeq)
    table.join(srcTrgTable)
  }

  def generateLinkKey(link: Datalog.Link): IndexKey[_] = link match {
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

  def transitionNoPathTables(frame: Frame, np: Datalog.NoPath): Table[Value] = {
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

  def transitionUndefTables(frame: Frame, un: Datalog.Undef): Table[Value] = {
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

  def transitionExtCallTables(frame: Frame, ext: Datalog.ExtensionalCall): Table[Value] = {
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

  def transitionEqCompTables(frame: Frame, comp: Datalog.Compare): Table[Value] = {
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

  def transitionEqCompBothBound(table: Table[Value], col1: String, col2: String): Table[Value] = {
    val col1Index = table.columnIndex(col1)
    val col2Index = table.columnIndex(col2)
    table.filter { row =>
      row(col1Index) == row(col2Index)
    }
  }

  def transitionEqCompOneBound(table: Table[Value], boundCol: String, unboundCol: String): Table[Value] = {
    val colIndex = table.columnIndex(boundCol)
    table.expand(unboundCol, row => row(colIndex))
  }

  def transitionEqCompConstBound(table: Table[Value], col: String, v: Value): Table[Value] = {
    val colIdx = table.columnIndex(col)
    table.filter { row =>
      row(colIdx) == v
    }
  }

  def transitionEqCompConstUnbound(table: Table[Value], col: String, v: Value): Table[Value] = {
    table.bind(col, v)
  }

  def transitionNeqCompTables(frame: Frame, comp: Datalog.Compare): Table[Value] = {
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

  def transitionNeqCompBothBound(table: Table[Value], col1: String, col2: String): Table[Value] = {
    val col1Idx = table.columnIndex(col1)
    val col2Idx = table.columnIndex(col2)
    table.filter { row =>
      row(col1Idx) != row(col2Idx)
    }
  }

  def transitionNeqCompOneConstant(table: Table[Value], col: String, v: Value): Table[Value] = {
    val colIdx = table.columnIndex(col)
    table.filter { row =>
      row(colIdx) != v
    }
  }

  def transitionReturnCallTables(callerFrame: Frame, name: String, argsTable: Table[Value], neg: Boolean = false): Frame.Tables = {
    val patternTable = fixpointState.relation(name, argsTable)
    val params = compiled.ir.patternMap(name).params.map(_.name)
    if (neg)
      transitionReturnNegCallTables(callerFrame, params, patternTable)
    else {
      transitionReturnCallTables(callerFrame, params, patternTable)
    }
  }

  def transitionReturnCallTables(callerFrame: Frame, calleeFrame: Frame): Frame.Tables = {
    val pat = calleeFrame.cp.point.pat
    val params = pat.params.map(_.name)
    val patternTable = fixpointState.relation(pat.name, calleeFrame.argsTable)
    transitionReturnCallTables(callerFrame, params, patternTable)
  }

  def transitionReturnCallTables(callerFrame: Frame, params: Seq[String], patternTable: Table[Value]): Frame.Tables = {
    // join bodyTable of caller with pattern table of callee
    val (_, args) = callerFrame.cp.atom.asCall.get
    val callArgVars = args.collect { case Datalog.Var(name) => name }
    val columnsSubst = params.zip(callArgVars).toMap
    val renamedPatternTable = patternTable.renameColumns(columnsSubst)
    val bodyTable = callerFrame.bodyTable.join(renamedPatternTable)

    (callerFrame.argsTable, bodyTable)
  }

  def transitionReturnNegCallTables(callerFrame: Frame, calleFrame: Frame): Frame.Tables = {
    val pat = calleFrame.cp.point.pat
    val params = pat.params.map(_.name)
    val patternTable = fixpointState.relation(pat.name, calleFrame.argsTable)
    transitionReturnNegCallTables(callerFrame, params, patternTable)
  }

  def transitionReturnNegCallTables(callerFrame: Frame, params: Seq[String], patternTable: Table[Value]): Frame.Tables = {
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

  def transitionCountAggTables(callerFrame: Frame, patternTable: Table[Value], lhs: Datalog.Term): Frame.Tables = {
    val count = patternTable.numRows
    transitionAggTables(callerFrame, lhs, ScalaValue(count))
  }

  def transitionCustomAggTables(callerFrame: Frame, patternTable: Table[Value], lhs: Datalog.Term, agg: CustomAggregation) : Frame.Tables = {
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

  def getInitValueAndJoin(agg: Scala[meta.Term]): (Scala[meta.Term], Scala[meta.Term]) = {
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

  def transitionAggTables(callerFrame: Frame, lhs: Datalog.Term, v: ScalaValue): Frame.Tables = {
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

  def transLiteral(c: Datalog.Literal): Value = c match {
    case Datalog.IntLiteral(v) => ScalaValue(v)
    case Datalog.LongLiteral(v) => ScalaValue(v)
    case Datalog.DoubleLiteral(v) => ScalaValue(v)
    case Datalog.StringLiteral(v) => ScalaValue(v)
    case Datalog.BooleanLiteral(v) => ScalaValue(v)
  }

  def transitionEvalTables(frame: Frame, lhs: Datalog.Term, eval: Datalog.Evaluation): Table[Value] = {
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



  def executeScala(table: Table[Value], row: Seq[Value], eval: Datalog.Evaluation): ScalaValue = {
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

  def executeScala(term: Scala[meta.Term]): ScalaValue =
    executeScala(term.syntax)

  def executeScala(term: String): ScalaValue = {
    val code = s"import ${defintionObjSym}.${compiled.name}._\n$term"
    ScalaValue(scalaCompiler.compileAndLoadScala(code))
  }
}
