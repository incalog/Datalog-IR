package inca.debugger.redesign_old

//import inca.backend.ir.Datalog
//import inca.backend.ir.Datalog.base
//import inca.backend.ir.Datalog.CustomAggregation
//import inca.compiler.CompiledModule
//import inca.debugger.table.indexing.IndexCover
//import inca.debugger.table.ImmutableTable
//import inca.debugger.table.IndexedTableFactory
//import inca.debugger.IllegalDebugStateException
//import inca.debugger.ScalaValue
//import inca.debugger.Value
//import inca.runtime.db.Database
//import inca.runtime.index._
//import inca.runtime.index.dynamic.ParentIndex
//import inca.runtime.index.virtual.NodeNotLinkedIndex
//import inca.runtime.index.virtual.NotNodeTypeIndex
//import inca.runtime.index.virtual.SizeIndex
//import inca.util.Gensym
//import inca.util.Scala
//import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
//import org.eclipse.viatra.query.runtime.matchers.tuple.TupleMask
//import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
//import scala.jdk.CollectionConverters._

//class TableOps(
//    var database: Database,
//    val compiled: CompiledModule,
//    val fixpointState: FixpointState[Value],
//    val indexedTableFactory: IndexedTableFactory[Value]
//  )(implicit valueOrdering: Ordering[Value],
//    topAndBotFactory: () => (Value, Value)) {
//
//  // Needed to execute scala code via reflection
//  private lazy val scalaCompiler = new Scala.ScalaCompiler()
//  private lazy val defintionObjSym: String = scalaCompiler.define {
//    import scala.meta._
//    q"object O {..${compiled.psystemSource.stats}}".syntax
//  }
//
//  def getDefinitionObjSym: String = defintionObjSym
//  def compileAndLoadScala[A](source: String): A =
//    scalaCompiler.compileAndLoadScala(source)
//
//  // Methods to prepare frame tables for atoms that can jump into another pattern (calls and aggregations)
//  def prepareArgTableOfCall(
//      frame: Frame,
//      calledPattern: Datalog.Pattern,
//      args: Seq[Datalog.Term],
//      resultIndices: Set[IndexCover] = Set()
//    ): ImmutableTable[Value] = {
//    val params = calledPattern.params.map(_.name)
//
//    // prepare argsTable
//    val paramSubst = params.zip(args)
//    val (varsBindings, constBindings) = paramSubst.partition(_._2.isInstanceOf[Datalog.Var])
//    val varsBindingsCast = varsBindings.map { case (p, v) => (p, v.asInstanceOf[Datalog.Var]) }
//    val constBindingsCast = constBindings.map { case (p, v) =>
//      (p, v.asInstanceOf[Datalog.Constant])
//    }
//    val columnsSubst = varsBindingsCast.map { case (p, v) => (v.name, p) }.toMap
//
//    val argsTable = frame.bodyTable.projectAndRename(columnsSubst)
//
//    if (constBindingsCast.isEmpty)
//      indexedTableFactory(argsTable.columns, argsTable.entries, resultIndices)
//    else {
//      // construct constants table
//      val constTable = indexedTableFactory(
//        constBindingsCast.map(_._1),
//        Seq(constBindingsCast.map(x => transLiteral(x._2.lit))),
//        argsTable)
//      // join with constants table
//      argsTable.join(constTable, resultIndices)
//    }
//  }
//
//  // methods to prepare frame tables for atoms that do not jump into another pattern (atom is not a call, or aggregation)
//  def transitionAtomTables(frame: Frame, atom: Datalog.Atom): ImmutableTable[Value] = atom match {
//    case ht: Datalog.HasType =>
//      transitionHasTypeTables(frame, ht)
//    case nht: Datalog.NotHasType =>
//      transitionNotHasTypeTables(frame, nht)
//    case comp @ Datalog.Compare(Datalog.EqComparator, _, _) =>
//      transitionEqCompTables(frame, comp)
//    case comp @ Datalog.Compare(Datalog.NeqComparator, _, _) =>
//      transitionNeqCompTables(frame, comp)
//    case p: Datalog.Path =>
//      transitionPathTables(frame, p)
//    case np: Datalog.NoPath =>
//      transitionNoPathTables(frame, np)
//    case un: Datalog.Undef =>
//      transitionUndefTables(frame, un)
//    case ext: Datalog.ExtensionalCall =>
//      transitionExtCallTables(frame, ext)
//    case Datalog.Computed(lhs, eval: Datalog.Evaluation) =>
//      transitionEvalTables(frame, lhs, eval)
//    case Datalog.Call(_, _, _, _) =>
//      throw IllegalDebugStateException(
//        s"The function transitionAtomTables should not be called with call atom $atom"
//      )
//    case Datalog.Computed(_, _) =>
//      throw IllegalDebugStateException(
//        s"The function transitionAtomTables should not be called with computed atom $atom"
//      )
//  }
//
//  // method to prepare frame tables of atoms that query unary edb relations (has type and not has type)
//  def transitionHasTypeTables(frame: Frame, ht: Datalog.HasType): ImmutableTable[Value] =
//    ht.t match {
//      case Datalog.Var(name) =>
//        val key = NodeTypeKey(transType(ht.typ))
//        transitionUnaryIndexTable(frame.bodyTable, name, key)
//      case Datalog.Constant(_) =>
//        throw new IllegalArgumentException(s"HasType is not defined on constants $ht")
//    }
//
//  def transType(typ: Datalog.Type): truechange.Type = typ match {
//    case Datalog.TAny => truechange.AnyType
//    case Datalog.TNode(name) => truechange.SortType(name)
//    case Datalog.TList(ty) => truechange.ListType(transType(ty))
//    case _ => throw new IllegalArgumentException("NOT SUPPORTED YET")
//  }
//
//  def transitionNotHasTypeTables(frame: Frame, nht: Datalog.NotHasType): ImmutableTable[Value] =
//    nht.t match {
//      case Datalog.Var(name) =>
//        val key = NotNodeTypeIndex.Key(transType(nht.typ))
//        transitionUnaryIndexTable(frame.bodyTable, name, key)
//      case Datalog.Constant(_) =>
//        throw new IllegalArgumentException(s"HasType is not defined on constants $nht")
//    }
//
//  def transitionUnaryIndexTable(
//      table: ImmutableTable[Value],
//      col: String,
//      key: IInputKey
//    ): ImmutableTable[Value] = {
//    if (table.isBound(col)) {
//      val colIdx = table.columnIndex(col)
//      table.select { row =>
//        val v = row(colIdx).unwrap
//        database.containsTuple(key, Tuples.staticArityFlatTupleOf(v))
//      }
//    } else {
//      val vals =
//        database.enumerateValues(key, TupleMask.empty(0), Tuples.staticArityFlatTupleOf()).asScala
//      val nameTable = indexedTableFactory(Seq(col), vals.toSeq.map(v => Seq(Value(v))), table)
//      table.join(nameTable)
//    }
//  }
//
//  // methods to prepare frame tables for atoms that query binary edb relations (path, nopath)
//  def transitionPathTables(frame: Frame, p: Datalog.Path): ImmutableTable[Value] = {
//    val (src, trg) = (p.src, p.trg) match {
//      case (Datalog.Var(srcName), Datalog.Var(trgName)) => (srcName, trgName)
//      case _ => throw IllegalDebugStateException(s"Path is not defined on constants $p")
//    }
//    val key = generateLinkKey(p.link)
//    val bodyTable = frame.bodyTable
//    val nextBodyTable =
//      if (bodyTable.isBound(src) && bodyTable.isBound(trg))
//        transitionBinaryIndexQueryBothBound(bodyTable, key, src, trg)
//      else if (bodyTable.isBound(src) && !bodyTable.isBound(trg))
//        transitionBinaryIndexQueryOneBound(bodyTable, key, src, trg, isSourceBound = true)
//      else if (!bodyTable.isBound(src) && bodyTable.isBound(trg))
//        transitionBinaryIndexQueryOneBound(bodyTable, key, trg, src, isSourceBound = false)
//      else
//        transitionBinaryIndexQueryUnbound(bodyTable, key, trg, src)
//    nextBodyTable
//  }
//
//  def transitionBinaryIndexQueryBothBound(
//      table: ImmutableTable[Value],
//      key: IInputKey,
//      src: String,
//      trg: String
//    ): ImmutableTable[Value] = {
//    val idxL = table.columnIndex(src)
//    val idxR = table.columnIndex(trg)
//    table.select { row =>
//      val vL = row(idxL).unwrap
//      val vR = row(idxR).unwrap
//      database.containsTuple(key, Tuples.staticArityFlatTupleOf(vL, vR))
//    }
//  }
//
//  def transitionBinaryIndexQueryOneBound(
//      table: ImmutableTable[Value],
//      key: IInputKey,
//      bound: String,
//      unbound: String,
//      isSourceBound: Boolean
//    ): ImmutableTable[Value] = {
//    val selectIdx = if (isSourceBound) 0 else 1
//    val mask = TupleMask.selectSingle(selectIdx, 2)
//    val boundIdx = table.columnIndex(bound)
//    val extendedEntries = table.entries.map { tuple =>
//      val boundV = tuple(boundIdx).unwrap
//      val unboundURI = database.enumerateValues(
//        key,
//        mask,
//        Tuples.staticArityFlatTupleOf(boundV)
//      ).iterator().next()
//      tuple :+ Value(unboundURI)
//    }
//    ImmutableTable(table.columns :+ unbound, extendedEntries)
//  }
//
//  def transitionBinaryIndexQueryUnbound(
//      table: ImmutableTable[Value],
//      key: IndexKey[_],
//      src: String,
//      trg: String
//    ): ImmutableTable[Value] = {
//    val rows = database.enumerateTuples(
//      key,
//      TupleMask.empty(2),
//      Tuples.staticArityFlatTupleOf()
//    ).iterator().asScala.map { tuple =>
//      val vL = tuple.get(0)
//      val vR = tuple.get(1)
//      Seq(Value(vL), Value(vR))
//    }
//    val columns = Seq(src, trg)
//    val srcTrgTable = indexedTableFactory(columns, rows.toSeq, table)
//    table.join(srcTrgTable)
//  }
//
//  def generateLinkKey(link: Datalog.Link): IndexKey[_] = link match {
//    case Datalog.ParentLink => ParentIndex.Key
//    case Datalog.NextLink => LinkListNextKey
//    case Datalog.SizeLink => SizeIndex.Key
//    case Datalog.NamedLink(node, field) =>
//      val link = (node.name, field)
//      if (compiled.dataModel.links.contains(link))
//        LinkNodeKey(link)
//      else
//        LinkPrimitiveKey(link)
//  }
//
//  def transitionNoPathTables(frame: Frame, np: Datalog.NoPath): ImmutableTable[Value] = {
//    val nodeKey = NodeTypeKey(transType(np.ty))
//    val linkKey = generateLinkKey(np.link)
//    val key = NodeNotLinkedIndex.Key(nodeKey, linkKey, np.termIsSource)
//    val bodyTable = np.t match {
//      case Datalog.Var(name) =>
//        if (frame.bodyTable.isBound(name))
//          transitionUnaryIndexTable(frame.bodyTable, name, key)
//        else
//          throw IllegalDebugStateException(
//            "Cannot debug NoPath atom where the given variable is unbound"
//          )
//      case Datalog.Constant(lit) =>
//        throw IllegalDebugStateException(
//          "Cannot debug NoPath atom where the given term is a constant"
//        )
//    }
//    bodyTable
//  }
//
//  def transitionUndefTables(frame: Frame, un: Datalog.Undef): ImmutableTable[Value] = {
//    val bodyTable = un.t match {
//      case Datalog.Var(name) =>
//        if (frame.bodyTable.isBound(name))
//          ImmutableTable.empty[Value](frame.bodyTable.columns)
//        else
//          frame.bodyTable
//      case Datalog.Constant(_) =>
//        ImmutableTable.empty[Value](frame.bodyTable.columns)
//    }
//    bodyTable
//  }
//
//  def transitionExtCallTables(frame: Frame, ext: Datalog.ExtensionalCall): ImmutableTable[Value] = {
//    val key = NamedRelationKey(ext.name, ext.args.size)
//
//    val gensym = new Gensym(Set())
//    val selectedIndices = ext.args.zipWithIndex.flatMap {
//      case (Datalog.Var(name), idx) =>
//        gensym.register(name)
//        if (frame.bodyTable.isBound(name)) Some(idx)
//        else None
//      case (Datalog.Constant(_), idx) => Some(idx)
//    }
//    val mask = TupleMask.fromSelectedIndices(ext.args.size, selectedIndices.toArray)
//
//    var argsTable: ImmutableTable[Value] = ImmutableTable.unit()
//
//    ext.args.foreach {
//      case Datalog.Var(name) if frame.bodyTable.isBound(name) =>
//        val indexCovers =
//          indexedTableFactory.constructIndexCovers(frame.bodyTable, Seq(name))
//        val lhsTable = frame.bodyTable.project(Seq(name), indexCovers)
//        argsTable = argsTable.join(lhsTable)
//      case Datalog.Var(_) => // do nothing
//      case Datalog.Constant(l) =>
//        val newCol = gensym.fresh("const")
//        val indexCovers = indexedTableFactory.constructIndexCovers(argsTable, Seq(newCol))
//        val constantTable =
//          ImmutableTable(Seq(newCol), Seq(Seq(transLiteral(l))), indexCovers = indexCovers)
//        argsTable.join(constantTable)
//    }
//
//    val extCallRows = argsTable.entries.flatMap { row =>
//      val seed = Tuples.flatTupleOf(row.map(_.unwrap): _*)
//      database.enumerateTuples(key, mask, seed).asScala.map { tuple =>
//        tuple.getElements.toSeq.map(Value.apply)
//      }.toSeq
//    }
//    val extCallColumns = ext.args.map {
//      case Datalog.Var(name) => name
//      case Datalog.Constant(_) => gensym.fresh("const")
//    }
//    // TODO maybe we need to construct a good index for this already to have good projection performance
//    val extCallTable = ImmutableTable(extCallColumns, extCallRows)
//
//    val extVarArgs = ext.args.collect { case Datalog.Var(name) => name }
//    val indexCovers = indexedTableFactory.constructIndexCovers(extCallTable, extVarArgs)
//    val projectedExtCallTable = extCallTable.project(extVarArgs, indexCovers)
//    frame.bodyTable.join(projectedExtCallTable)
//  }
//
//  def transitionEqCompTables(frame: Frame, comp: Datalog.Compare): ImmutableTable[Value] = {
//    val bodyTable = frame.bodyTable
//    val nextBodyTable: ImmutableTable[Value] = (comp.lhs, comp.rhs) match {
//      case (Datalog.Var(name1), Datalog.Var(name2)) =>
//        if (bodyTable.isBound(name1) && bodyTable.isBound(name2))
//          transitionEqCompBothBound(bodyTable, name1, name2)
//        else if (bodyTable.isBound(name1) && !bodyTable.isBound(name2))
//          transitionEqCompOneBound(bodyTable, name1, name2)
//        else if (!bodyTable.isBound(name1) && bodyTable.isBound(name2))
//          transitionEqCompOneBound(bodyTable, name2, name1)
//        else
//          throw IllegalDebugStateException(
//            "Cannot debug eq comparator where both arguments are not bound"
//          )
//      case (Datalog.Var(name), Datalog.Constant(l)) =>
//        if (bodyTable.isBound(name))
//          transitionEqCompConstBound(bodyTable, name, transLiteral(l))
//        else
//          transitionEqCompConstUnbound(bodyTable, name, transLiteral(l))
//      case (Datalog.Constant(l), Datalog.Var(name)) =>
//        if (bodyTable.isBound(name))
//          transitionEqCompConstBound(bodyTable, name, transLiteral(l))
//        else
//          transitionEqCompConstUnbound(bodyTable, name, transLiteral(l))
//      case (Datalog.Constant(l1), Datalog.Constant(l2)) =>
//        val v1 = transLiteral(l1)
//        val v2 = transLiteral(l2)
//        if (v1 == v2) frame.bodyTable
//        else ImmutableTable.empty(frame.bodyTable.columns)
//    }
//    nextBodyTable
//  }
//
//  def transitionEqCompBothBound(
//      table: ImmutableTable[Value],
//      col1: String,
//      col2: String
//    ): ImmutableTable[Value] = {
//    val col1Index = table.columnIndex(col1)
//    val col2Index = table.columnIndex(col2)
//    table.select { row =>
//      row(col1Index) == row(col2Index)
//    }
//  }
//
//  def transitionEqCompOneBound(
//      table: ImmutableTable[Value],
//      boundCol: String,
//      unboundCol: String
//    ): ImmutableTable[Value] = {
//    val colIndex = table.columnIndex(boundCol)
//    val extendedEntries = table.entries.map { tuple =>
//      tuple :+ tuple(colIndex)
//    }
//    ImmutableTable(table.columns :+ unboundCol, extendedEntries)
//  }
//
//  def transitionEqCompConstBound(
//      table: ImmutableTable[Value],
//      col: String,
//      v: Value
//    ): ImmutableTable[Value] = {
//    val colIdx = table.columnIndex(col)
//    table.select { row =>
//      row(colIdx) == v
//    }
//  }
//
//  def transitionEqCompConstUnbound(
//      table: ImmutableTable[Value],
//      col: String,
//      v: Value
//    ): ImmutableTable[Value] = {
//    val indexCovers = indexedTableFactory.constructIndexCovers(table, Seq(col))
//    val singleValueTable = ImmutableTable(Seq(col), Seq(Seq(v)), indexCovers = indexCovers)
//    table.join(singleValueTable)
//  }
//
//  def transitionNeqCompTables(frame: Frame, comp: Datalog.Compare): ImmutableTable[Value] = {
//    val bodyTable = frame.bodyTable
//    val nextBodyTable: ImmutableTable[Value] = (comp.lhs, comp.rhs) match {
//      case (Datalog.Var(name1), Datalog.Var(name2)) =>
//        if (bodyTable.isBound(name1) && bodyTable.isBound(name2))
//          transitionNeqCompBothBound(bodyTable, name1, name2)
//        else
//          throw IllegalDebugStateException(
//            "Cannot debug neq comparator where one argument is not bound"
//          )
//      case (Datalog.Var(name), Datalog.Constant(l)) =>
//        if (bodyTable.isBound(name))
//          transitionNeqCompOneConstant(bodyTable, name, transLiteral(l))
//        else
//          throw IllegalDebugStateException(
//            "Cannot debug neq comparator where one argument is not bound"
//          )
//      case (Datalog.Constant(l), Datalog.Var(name)) =>
//        if (bodyTable.isBound(name))
//          transitionNeqCompOneConstant(bodyTable, name, transLiteral(l))
//        else
//          throw IllegalDebugStateException(
//            "Cannot debug neq comparator where one argument is not bound"
//          )
//      case (Datalog.Constant(l1), Datalog.Constant(l2)) =>
//        val v1 = transLiteral(l1)
//        val v2 = transLiteral(l2)
//        if (v1 == v2) frame.bodyTable
//        else ImmutableTable.empty(bodyTable.columns)
//    }
//    nextBodyTable
//  }
//
//  def transitionNeqCompBothBound(
//      table: ImmutableTable[Value],
//      col1: String,
//      col2: String
//    ): ImmutableTable[Value] = {
//    val col1Idx = table.columnIndex(col1)
//    val col2Idx = table.columnIndex(col2)
//    table.select { row =>
//      row(col1Idx) != row(col2Idx)
//    }
//  }
//
//  def transitionNeqCompOneConstant(
//      table: ImmutableTable[Value],
//      col: String,
//      v: Value
//    ): ImmutableTable[Value] = {
//    val colIdx = table.columnIndex(col)
//    table.select { row =>
//      row(colIdx) != v
//    }
//  }
//
//  def transitionReturnCallTables(
//      callerFrame: Frame,
//      name: String,
//      argsTable: ImmutableTable[Value],
//      neg: Boolean = false
//    ): Frame.Tables = {
//    val patternTable = fixpointState.relation(name, argsTable)
//    val params = compiled.ir.patternMap(name).params.map(_.name)
//    if (neg)
//      transitionReturnNegCallTables(callerFrame, params, patternTable)
//    else {
//      transitionReturnCallTables(callerFrame, params, patternTable)
//    }
//  }
//
//  def transitionReturnCallTables(callerFrame: Frame, calleeFrame: Frame): Frame.Tables = {
//    val pat = calleeFrame.cp.point.pat
//    val params = pat.params.map(_.name)
//    val patternTable = fixpointState.relation(pat.name, calleeFrame.argsTable)
//    transitionReturnCallTables(callerFrame, params, patternTable)
//  }
//
//  def transitionReturnCallTables(
//      callerFrame: Frame,
//      params: Seq[String],
//      patternTable: ImmutableTable[Value]
//    ): Frame.Tables = {
//    // join bodyTable of caller with pattern table of callee
//    val (_, args) = callerFrame.cp.atom.asCall.get
//    val columnsSubst = params.zip(args).flatMap {
//      case (param, Datalog.Var(argName)) => Some(param -> argName)
//      case _ => None
//    }.toMap
//
//    val renamedColumnsOfPatternTable = patternTable.columns.flatMap(columnsSubst.get)
//    val indexCovers =
//      indexedTableFactory.constructIndexCovers(callerFrame.bodyTable, renamedColumnsOfPatternTable)
//
//    val renamedPatternTable =
//      patternTable.projectAndRename(columnsSubst, indexCovers)
//    val bodyTable = callerFrame.bodyTable.join(renamedPatternTable)
//
//    (callerFrame.argsTable, bodyTable)
//  }
//
//  def transitionReturnNegCallTables(callerFrame: Frame, calleFrame: Frame): Frame.Tables = {
//    val pat = calleFrame.cp.point.pat
//    val params = pat.params.map(_.name)
//    val patternTable = fixpointState.relation(pat.name, calleFrame.argsTable)
//    transitionReturnNegCallTables(callerFrame, params, patternTable)
//  }
//
//  def prepareTransitionReturnCallTable(
//      callerFrame: Frame,
//      params: Seq[String],
//      patternTable: ImmutableTable[Value]
//    ): ImmutableTable[Value] = {
//    val (_, args) = callerFrame.cp.atom.asCall.get
//    val columnsSubst = params.zip(args).flatMap {
//      case (param, Datalog.Var(lit)) => Some(param -> lit)
//      case _ => None
//    }.toMap
//    val (constantArgParams, constantArgLits) = params.zip(args).flatMap {
//      case (param, Datalog.Constant(lit)) => Some(param -> lit)
//      case _ => None
//    }.unzip
//    val constantsTable =
//      ImmutableTable[Value](constantArgParams, Seq(constantArgLits.map(transLiteral)))
//
//    // construct index such that joinin with frame.bodyTable is efficient
//    val indexCovers = indexedTableFactory.constructIndexCovers(
//      callerFrame.bodyTable,
//      // this is just params
//      patternTable.columns.flatMap(columnsSubst.get) ++ constantArgParams)
//
//    // first rename than join with constant arguments
//    patternTable.projectAndRename(columnsSubst).join(constantsTable, resultIndices = indexCovers)
//
//  }
//  def transitionReturnNegCallTables(
//      callerFrame: Frame,
//      params: Seq[String],
//      patternTable: ImmutableTable[Value]
//    ): Frame.Tables = {
//    val preparedPatternTable = prepareTransitionReturnCallTable(callerFrame, params, patternTable)
//    // remove rows of caller bodyTable of that contains tuples of pattern table of callee
//    val bodyTable = callerFrame.bodyTable.antiJoin(preparedPatternTable)
//    (callerFrame.argsTable, bodyTable)
//  }
//
//  def transitionCountAggTables(
//      callerFrame: Frame,
//      patternTable: ImmutableTable[Value],
//      lhs: Datalog.Term
//    ): Frame.Tables = {
//    val count = patternTable.size
//    transitionAggTables(callerFrame, lhs, ScalaValue(count))
//  }
//
//  def transitionCustomAggTables(
//      callerFrame: Frame,
//      patternTable: ImmutableTable[Value],
//      lhs: Datalog.Term,
//      agg: CustomAggregation
//    ): Frame.Tables = {
//    val valsToAgg = patternTable.entries.map { row =>
//      row(agg.aggregatedColumn).asScala
//    }.toSeq
//    val (initTerm, joinOpTerm) = getInitValueAndJoin(agg.agg)
//    val initValue = executeScala(initTerm)
//    val foldRes = valsToAgg.foldLeft(initValue.v) { case (res, x) =>
//      // currently we assume that the previous result is the left op and the current value the right op
//      val joinCode = s"(${joinOpTerm.syntax})($res, $x)"
//      executeScala(joinCode).v
//    }
//    transitionAggTables(callerFrame, lhs, ScalaValue(foldRes))
//  }
//
//  def getInitValueAndJoin(agg: Scala[meta.Term]): (Scala[meta.Term], Scala[meta.Term]) = {
//    import scala.meta._
//    var init: Option[meta.Term] = None
//    var joinOp: Option[meta.Term] = None
//    agg.tree match {
//      case Term.NewAnonymous(Template(_, _, _, stats)) =>
//        stats.foreach {
//          case Defn.Def(_, Term.Name("init"), _, _, _, body) =>
//            init = Some(body)
//          case Defn.Def(_, Term.Name("join"), _, Seq(params), _, body) =>
//            joinOp = Some(q"(..$params) => $body")
//          case _ => // do nothing
//        }
//      case _ =>
//        throw IllegalDebugStateException(
//          "Object created for aggregation is not of type Aggregation"
//        )
//    }
//    if (init.isEmpty || joinOp.isEmpty) {
//      throw IllegalDebugStateException("Aggregation has no initial value or join operation defined")
//    }
//    (Scala(init.get), Scala(joinOp.get))
//  }
//
//  def transitionAggTables(callerFrame: Frame, lhs: Datalog.Term, v: ScalaValue): Frame.Tables = {
//    val table = callerFrame.bodyTable
//    val extBodyTable = lhs match {
//      case Datalog.Var(name) =>
//        if (table.isBound(name)) {
//          val nameIdx = table.columnIndex(name)
//          table.select { row =>
//            row(nameIdx) == v
//          }
//        } else {
//          val indexCovers = indexedTableFactory.constructIndexCovers(table, Seq(name))
//          val singleValueTable: ImmutableTable[Value] =
//            ImmutableTable(Seq(name), Seq(Seq(v)), indexCovers = indexCovers)
//          table.join(singleValueTable)
//        }
//      case Datalog.Constant(lit) =>
//        val transLit = transLiteral(lit)
//        if (transLit == v)
//          table
//        else
//          ImmutableTable.empty[Value](table.columns)
//    }
//    (callerFrame.argsTable, extBodyTable)
//  }
//
//  def transLiteral(c: Datalog.base.Literal): Value = c match {
//    case Datalog.base.IntLiteral(v) => ScalaValue(v)
//    case Datalog.base.LongLiteral(v) => ScalaValue(v)
//    case Datalog.base.DoubleLiteral(v) => ScalaValue(v)
//    case Datalog.base.StringLiteral(v) => ScalaValue(v)
//    case Datalog.base.BooleanLiteral(v) => ScalaValue(v)
//  }
//
//  def transitionEvalTables(
//      frame: Frame,
//      lhs: Datalog.Term,
//      eval: Datalog.Evaluation
//    ): ImmutableTable[Value] = {
//    val bodyTable = frame.bodyTable
//
//    val lhsValue: Seq[Value] => Value = lhs match {
//      case Datalog.Var(name) =>
//        if (bodyTable.isBound(name)) {
//          val cix = bodyTable.columnIndex(name)
//          row => row(cix)
//        } else { _ =>
//          throw new IllegalArgumentException
//        }
//      case Datalog.Constant(lit) =>
//        val v = transLiteral(lit)
//        _ => v
//    }
//
//    lhs match {
//      case Datalog.Var(name) if !bodyTable.isBound(name) =>
//        val extendedEntries = bodyTable.entries.map { tuple =>
//          tuple :+ executeScala(bodyTable, tuple, eval)
//        }
//        ImmutableTable[Value](bodyTable.columns :+ name, extendedEntries)
//      case _ =>
//        bodyTable.select { tuple =>
//          val scalaValue = executeScala(bodyTable, tuple, eval)
//          val lhsVal = lhsValue(tuple)
//          scalaValue == lhsVal
//        }
//    }
//  }
//
//  def executeScala(
//      table: ImmutableTable[Value],
//      row: Seq[Value],
//      eval: Datalog.Evaluation
//    ): ScalaValue = {
//    val argTerms = eval.evalArgs.map {
//      case (Datalog.Var(v), ty) => s"""$$env("$v").asInstanceOf[${base.typeAsScala(ty).syntax}]"""
//      case (Datalog.Constant(lit), _) =>
//        lit match {
//          case Datalog.base.IntLiteral(v) => v.toString
//          case Datalog.base.LongLiteral(v) => v.toString
//          case Datalog.base.DoubleLiteral(v) => v.toString
//          case Datalog.base.StringLiteral(v) => v.toString
//          case Datalog.base.BooleanLiteral(v) => v.toString
//        }
//    }
//    val argsMap: Map[String, Any] = eval.evalArgs.flatMap {
//      case (Datalog.Var(v), _) => Some(v -> row(table.columnIndex(v)).unwrap)
//      case (Datalog.Constant(_), _) => None
//    }.toMap
//
//    val funCode =
//      s"""{ ($$env: Map[String, Any]) =>
//        |  import ${defintionObjSym}.${compiled.name}._
//        |  (${eval.code.syntax})(${argTerms.mkString(", ")})
//        |}""".stripMargin
//
//    val fun: Map[String, Any] => Any =
//      scalaCompiler.compileAndLoadScala[Map[String, Any] => Any](funCode)
//    ScalaValue(fun(argsMap))
//  }
//
//  def executeScala(term: Scala[meta.Term]): ScalaValue =
//    executeScala(term.syntax)
//
//  def executeScala(term: String): ScalaValue = {
//    val code = s"import ${defintionObjSym}.${compiled.name}._\n$term"
//    ScalaValue(scalaCompiler.compileAndLoadScala[Any](code))
//  }
//}
