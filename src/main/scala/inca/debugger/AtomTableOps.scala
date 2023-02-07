package inca.debugger

import inca.backend.ir.CollectVarNames
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.base
import inca.debugger.table.IndexedTableFactory
import inca.debugger.AtomTableOps.transLiteral
import inca.debugger.AtomTableOps.transType
import inca.embedded.Datalog
import inca.runtime.index.dynamic.ParentIndex
import inca.runtime.index.virtual.NodeNotLinkedIndex
import inca.runtime.index.virtual.NotNodeTypeIndex
import inca.runtime.index.virtual.SizeIndex
import inca.runtime.index.IndexKey
import inca.runtime.index.LinkListNextKey
import inca.runtime.index.LinkNodeKey
import inca.runtime.index.LinkPrimitiveKey
import inca.runtime.index.NamedRelationKey
import inca.runtime.index.NodeTypeKey
import inca.runtime.DatalogRuntime
import inca.util.Gensym
import inca.util.Scala
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
import org.eclipse.viatra.query.runtime.matchers.tuple.TupleMask
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.jdk.CollectionConverters.IteratorHasAsScala

class AtomTableOps(val runtime: DatalogRuntime, indexedTableFactory: IndexedTableFactory[Value]) {

  // Needed to execute scala code via reflection
  private lazy val scalaCompiler = new Scala.ScalaCompiler()
  private lazy val definitionObjSym: String = scalaCompiler.define {
    import scala.meta._
    q"object O {..${runtime.compiled.psystemSource.stats}}".syntax
  }

  def getDefinitionObjSym: String = definitionObjSym
  def compileAndLoadScala[A](source: String): A = scalaCompiler.compileAndLoadScala(source)

  def atom(t: ValueTable, atom: Datalog.Atom): ValueTable = atom match {
    case c @ Datalog.Compare(Datalog.EqComparator, _, _) => eq(t, c)
    case c @ Datalog.Compare(Datalog.NeqComparator, _, _) => neq(t, c)
    case ht: Datalog.HasType => hasType(t, ht)
    case nht: Datalog.NotHasType => notHasType(t, nht)
    case p: Datalog.Path => path(t, p)
    case np: Datalog.NoPath => noPath(t, np)
    case u: Datalog.Undef => undef(t, u)
    case ec: Datalog.ExtensionalCall => extensionalCall(t, ec)
    case c: Datalog.Computed => computed(t, c)
    case c: Datalog.Call =>
      throw IllegalDebugStateException(s"We dont process calls with AtomTableOps, but got $c")
  }

  private def eq(t: ValueTable, atom: Datalog.Compare): ValueTable =
    (atom.lhs, atom.rhs) match {
      case (Datalog.Var(name1), Datalog.Var(name2)) =>
        if (t.isBound(name1) && t.isBound(name2)) eqBothBound(t, name1, name2)
        else if (t.isBound(name1) && !t.isBound(name2)) eqOneBound(t, name1, name2)
        else if (!t.isBound(name1) && t.isBound(name2)) eqOneBound(t, name2, name1)
        else
          throw IllegalDebugStateException(
            "Cannot debug eq comparator where both arguments are not bound"
          )
      case (Datalog.Var(name), Datalog.Constant(l)) =>
        if (t.isBound(name)) eqConstBound(t, name, transLiteral(l))
        else eqConstUnbound(t, name, transLiteral(l))
      case (Datalog.Constant(l), Datalog.Var(name)) =>
        if (t.isBound(name)) eqConstBound(t, name, transLiteral(l))
        else eqConstUnbound(t, name, transLiteral(l))
      case (Datalog.Constant(l1), Datalog.Constant(l2)) =>
        val v1 = transLiteral(l1)
        val v2 = transLiteral(l2)
        if (v1 == v2) t
        else ValueTable.empty(t.columns)
    }

  private def eqBothBound(t: ValueTable, col1: String, col2: String): ValueTable = {
    val col1Index = t.columnIndex(col1)
    val col2Index = t.columnIndex(col2)
    t.select(row => row(col1Index) == row(col2Index))
  }

  private def eqOneBound(t: ValueTable, boundCol: String, unboundCol: String): ValueTable = {
    val colIndex = t.columnIndex(boundCol)
    val extendedEntries = t.entries.map(tuple => tuple :+ tuple(colIndex))
    ValueTable(t.columns :+ unboundCol, extendedEntries)
  }

  private def eqConstBound(t: ValueTable, col: String, v: Value): ValueTable = {
    val colIdx = t.columnIndex(col)
    t.select(row => row(colIdx) == v)
  }

  private def eqConstUnbound(t: ValueTable, col: String, v: Value): ValueTable = {
    val indexCovers = indexedTableFactory.constructIndexCovers(t, Seq(col))
    val singleValueTable = ValueTable(Seq(col), Seq(Seq(v)), indexCovers = indexCovers)
    t.join(singleValueTable)
  }

  private def neq(t: ValueTable, atom: Datalog.Compare): ValueTable =
    (atom.lhs, atom.rhs) match {
      case (Datalog.Var(name1), Datalog.Var(name2)) =>
        if (t.isBound(name1) && t.isBound(name2))
          neqBothBound(t, name1, name2)
        else
          throw IllegalDebugStateException(
            "Cannot debug neq comparator where one argument is not bound"
          )
      case (Datalog.Var(name), Datalog.Constant(l)) =>
        if (t.isBound(name))
          neqOneConst(t, name, transLiteral(l))
        else
          throw IllegalDebugStateException(
            "Cannot debug neq comparator where one argument is not bound"
          )
      case (Datalog.Constant(l), Datalog.Var(name)) =>
        if (t.isBound(name))
          neqOneConst(t, name, transLiteral(l))
        else
          throw IllegalDebugStateException(
            "Cannot debug neq comparator where one argument is not bound"
          )
      case (Datalog.Constant(l1), Datalog.Constant(l2)) =>
        val v1 = transLiteral(l1)
        val v2 = transLiteral(l2)
        if (v1 == v2) t
        else ValueTable.empty(t.columns)
    }

  private def neqBothBound(t: ValueTable, col1: String, col2: String): ValueTable = {
    val col1Idx = t.columnIndex(col1)
    val col2Idx = t.columnIndex(col2)
    t.select(row => row(col1Idx) != row(col2Idx))
  }

  private def neqOneConst(t: ValueTable, col: String, v: Value): ValueTable = {
    val colIdx = t.columnIndex(col)
    t.select(row => row(colIdx) != v)
  }

  private def path(t: ValueTable, atom: Datalog.Path): ValueTable = {
    val (src, trg) = (atom.src, atom.trg) match {
      case (Datalog.Var(srcName), Datalog.Var(trgName)) => (srcName, trgName)
      case _ => throw IllegalDebugStateException(s"Path is not defined on constants $atom")
    }
    val key = generateLinkKey(atom.link)
    if (t.isBound(src) && t.isBound(trg))
      binaryBothBound(t, key, src, trg)
    else if (t.isBound(src) && !t.isBound(trg))
      binaryOneBound(t, key, src, trg, isSourceBound = true)
    else if (!t.isBound(src) && t.isBound(trg))
      binaryOneBound(t, key, trg, src, isSourceBound = false)
    else
      binaryNoneBound(t, key, trg, src)
  }

  private def binaryBothBound(
      t: ValueTable,
      key: IInputKey,
      src: String,
      trg: String
    ): ValueTable = {
    val idxL = t.columnIndex(src)
    val idxR = t.columnIndex(trg)
    t.select { row =>
      val vL = row(idxL).unwrap
      val vR = row(idxR).unwrap
      runtime.db.containsTuple(key, Tuples.staticArityFlatTupleOf(vL, vR))
    }
  }

  private def binaryOneBound(
      t: ValueTable,
      key: IInputKey,
      bound: String,
      unbound: String,
      isSourceBound: Boolean
    ): ValueTable = {
    val selectIdx = if (isSourceBound) 0 else 1
    val mask = TupleMask.selectSingle(selectIdx, 2)
    val boundIdx = t.columnIndex(bound)
    val extendedEntries = t.entries.map { tuple =>
      val boundV = tuple(boundIdx).unwrap
      val unboundURI = runtime.db.enumerateValues(
        key,
        mask,
        Tuples.staticArityFlatTupleOf(boundV)
      ).iterator().next()
      tuple :+ Value(unboundURI)
    }
    ValueTable(t.columns :+ unbound, extendedEntries)
  }

  private def binaryNoneBound(
      t: ValueTable,
      key: IndexKey[_],
      src: String,
      trg: String
    ): ValueTable = {
    val rows = runtime.db.enumerateTuples(
      key,
      TupleMask.empty(2),
      Tuples.staticArityFlatTupleOf()
    ).iterator().asScala.map { tuple =>
      val vL = tuple.get(0)
      val vR = tuple.get(1)
      Seq(Value(vL), Value(vR))
    }
    val columns = Seq(src, trg)
    val srcTrgTable = indexedTableFactory(columns, rows.toSeq, t)
    t.join(srcTrgTable)
  }

  private def generateLinkKey(link: Datalog.Link): IndexKey[_] = link match {
    case Datalog.ParentLink => ParentIndex.Key
    case Datalog.NextLink => LinkListNextKey
    case Datalog.SizeLink => SizeIndex.Key
    case Datalog.NamedLink(node, field) =>
      val link = (node.name, field)
      if (runtime.compiled.dataModel.links.contains(link))
        LinkNodeKey(link)
      else
        LinkPrimitiveKey(link)
  }

  private def noPath(t: ValueTable, atom: Datalog.NoPath): ValueTable = {
    val nodeKey = NodeTypeKey(transType(atom.ty))
    val linkKey = generateLinkKey(atom.link)
    val key = NodeNotLinkedIndex.Key(nodeKey, linkKey, atom.termIsSource)
    atom.t match {
      case Datalog.Var(name) =>
        if (t.isBound(name))
          unary(t, name, key)
        else
          throw IllegalDebugStateException(
            "Cannot debug NoPath atom where the given variable is unbound"
          )
      case Datalog.Constant(_) =>
        throw IllegalDebugStateException(
          "Cannot debug NoPath atom where the given term is a constant"
        )
    }
  }

  private def unary(t: ValueTable, col: String, key: IInputKey): ValueTable = {
    if (t.isBound(col)) {
      val colIdx = t.columnIndex(col)
      t.select { row =>
        val v = row(colIdx).unwrap
        runtime.db.containsTuple(key, Tuples.staticArityFlatTupleOf(v))
      }
    } else {
      val vals =
        runtime.db.enumerateValues(key, TupleMask.empty(0), Tuples.staticArityFlatTupleOf()).asScala
      val nameTable = indexedTableFactory(Seq(col), vals.toSeq.map(v => Seq(Value(v))), t)
      t.join(nameTable)
    }
  }

  private def hasType(t: ValueTable, atom: Datalog.HasType): ValueTable =
    atom.t match {
      case Datalog.Var(name) =>
        val key = NodeTypeKey(transType(atom.typ))
        unary(t, name, key)
      case Datalog.Constant(_) =>
        throw new IllegalArgumentException(s"HasType is not defined on constants $atom")
    }

  private def notHasType(t: ValueTable, atom: Datalog.NotHasType): ValueTable =
    atom.t match {
      case Datalog.Var(name) =>
        val key = NotNodeTypeIndex.Key(transType(atom.typ))
        unary(t, name, key)
      case Datalog.Constant(_) =>
        throw new IllegalArgumentException(s"HasType is not defined on constants $atom")
    }

  private def undef(t: ValueTable, atom: Datalog.Undef): ValueTable =
    atom.t match {
      case Datalog.Var(name) =>
        if (t.isBound(name)) ValueTable.empty(t.columns)
        else t
      case Datalog.Constant(_) =>
        ValueTable.empty(t.columns)
    }

  // TODO improve performance
  private def extensionalCall(t: ValueTable, atom: Datalog.ExtensionalCall): ValueTable = {
    val key = NamedRelationKey(atom.name, atom.args.size)
    val gensym = new Gensym(Datalog.collectVarNames.transAtom(atom))

    var argsTable: ValueTable = ValueTable.unit()
    atom.args.foreach {
      case Datalog.Var(name) if t.isBound(name) =>
        val indexCovers =
          indexedTableFactory.constructIndexCovers(t, Seq(name))
        val lhsTable = t.project(Seq(name), indexCovers)
        argsTable = argsTable.join(lhsTable)
      case Datalog.Var(_) => // do nothing
      case Datalog.Constant(l) =>
        val newCol = gensym.fresh("generated$$const$$")
        val indexCovers = indexedTableFactory.constructIndexCovers(argsTable, Seq(newCol))
        val constantTable =
          ValueTable(Seq(newCol), Seq(Seq(transLiteral(l))), indexCovers = indexCovers)
        argsTable.join(constantTable)
    }
    val mask = TupleMask.empty(atom.args.size)
    val extEntries = runtime.db.enumerateTuples(key, mask, null).asScala.map(
      _.getElements.toSeq.map(Value.apply)).toSeq
    val extCallColumns = atom.args.map {
      case Datalog.Var(name) => name
      case Datalog.Constant(_) => gensym.fresh("read$$const$$")
    }
    // TODO maybe we need to construct a good index for this already to have good projection performance
    val extCallTable = ValueTable(extCallColumns, extEntries)

    val extVarArgs = atom.args.collect { case Datalog.Var(name) => name }
    val sameCols = extVarArgs.filter(t.columns.contains)
    val indexCovers = indexedTableFactory.constructIndexCovers(extCallTable, sameCols)
    val projectedExtCallTable = extCallTable.project(extVarArgs, indexCovers)
    t.join(projectedExtCallTable)
  }

  private def computed(t: ValueTable, atom: Datalog.Computed): ValueTable =
    atom.computation match {
      case e: Datalog.Evaluation => eval(t, e, atom.lhs)
      case c: Datalog.CountAggregation => count(t, c, atom.lhs)
      case c: Datalog.CustomAggregation => custom(t, c, atom.lhs)
    }

  private def eval(t: ValueTable, comp: Datalog.Evaluation, lhs: Datalog.Term): ValueTable = {
    val lhsValue: Seq[Value] => Value = lhs match {
      case Datalog.Var(name) =>
        if (t.isBound(name)) {
          val cix = t.columnIndex(name)
          row => row(cix)
        } else { _ =>
          throw new IllegalArgumentException
        }
      case Datalog.Constant(lit) =>
        val v = transLiteral(lit)
        _ => v
    }

    lhs match {
      case Datalog.Var(name) if !t.isBound(name) =>
        val extendedEntries = t.entries.map { tuple =>
          tuple :+ executeScala(t, tuple, comp)
        }
        ValueTable(t.columns :+ name, extendedEntries)
      case _ =>
        t.select { tuple =>
          val scalaValue = executeScala(t, tuple, comp)
          val lhsVal = lhsValue(tuple)
          scalaValue == lhsVal
        }
    }
  }

  private def executeScala(t: ValueTable, row: Seq[Value], eval: Datalog.Evaluation): ScalaValue = {
    val argTerms = eval.evalArgs.map {
      case (Datalog.Var(v), ty) => s"""$$env("$v").asInstanceOf[${base.typeAsScala(ty).syntax}]"""
      case (Datalog.Constant(lit), _) =>
        lit match {
          case Datalog.base.IntLiteral(v) => v.toString
          case Datalog.base.LongLiteral(v) => v.toString
          case Datalog.base.DoubleLiteral(v) => v.toString
          case Datalog.base.StringLiteral(v) => v
          case Datalog.base.BooleanLiteral(v) => v.toString
        }
    }
    val argsMap: Map[String, Any] = eval.evalArgs.flatMap {
      case (Datalog.Var(v), _) => Some(v -> row(t.columnIndex(v)).unwrap)
      case (Datalog.Constant(_), _) => None
    }.toMap

    val funCode =
      s"""{ ($$env: Map[String, Any]) =>
        |  import $definitionObjSym.${runtime.compiled.name}._
        |  (${eval.code.syntax})(${argTerms.mkString(", ")})
        |}""".stripMargin

    val fun: Map[String, Any] => Any =
      scalaCompiler.compileAndLoadScala[Map[String, Any] => Any](funCode)
    ScalaValue(fun(argsMap))
  }

  private def count(t: ValueTable, comp: Datalog.CountAggregation, lhs: Datalog.Term): ValueTable =
    throw IllegalDebugStateException("Not supported yet")

  private def custom(
      t: ValueTable,
      comp: Datalog.CustomAggregation,
      lhs: Datalog.Term
    ): ValueTable =
    throw IllegalDebugStateException("Not supported yet")

}

object AtomTableOps {
  def transLiteral(c: Datalog.base.Literal): Value = c match {
    case Datalog.base.IntLiteral(v) => ScalaValue(v)
    case Datalog.base.LongLiteral(v) => ScalaValue(v)
    case Datalog.base.DoubleLiteral(v) => ScalaValue(v)
    case Datalog.base.StringLiteral(v) => ScalaValue(v)
    case Datalog.base.BooleanLiteral(v) => ScalaValue(v)
  }

  def transType(typ: Datalog.Type): truechange.Type = typ match {
    case Datalog.TAny => truechange.AnyType
    case Datalog.TNode(name) => truechange.SortType(name)
    case Datalog.TList(ty) => truechange.ListType(transType(ty))
    case _ => throw new IllegalArgumentException("NOT SUPPORTED YET")
  }
}
