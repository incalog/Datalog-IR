package inca.ir.extension.tuple.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.analysis.base.effect.{BaseIRFailure, InvalidBindings}
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, Index, NoIndex, SupColumn}
import inca.ir.extension.tuple.{Project, TupleLit}
import sturdy.data.{MakeJoined, MayJoin, mapJoin}
import sturdy.values.Topped

case object InvalidTupleProjection extends BaseIRFailure

trait TupleOps[V]:
  def tupleLit(ts: Seq[V]): V
  def project(t: V, index: Int): V
  def iter(t: V): Seq[V]

case class IndexPath(path: Seq[Int]) extends Index

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val tupleOps: TupleOps[V]

  override protected def canDetermineValue(t: Term): Boolean = t match
    case TupleLit(ts) => ts.forall(canDetermineValue)
    case Project(t, idx) => canDetermineValue(t)
    case _ => super.canDetermineValue(t)

  private var currentIndex: Seq[Int] = Seq()
  def scopedIndex[A](update: Seq[Int] => Seq[Int])(f: => A): A = {
    val oldIndexPath = this.currentIndex
    currentIndex = update(currentIndex)
    try {
      val a = f
      a
    } finally {
      this.currentIndex = oldIndexPath
    }
  }

  override protected def extractBindingColumns(term: ir.Term, index: Index = NoIndex): BindingColumns = term match
    case TupleLit(ts) =>
      val initial: BindingColumns = Seq()
      ts.zipWithIndex.foldLeft(initial) { case (acc, (t, i)) =>
        val binding = scopedIndex(_ :+ i)(extractBindingColumns(t, IndexPath(currentIndex)))
        acc ++ binding
      }
    case _ => super.extractBindingColumns(term, index)

  override protected def bindInSupplementary(to: Seq[SupColumn], from: (SupColumn, Seq[Index])): (Seq[SupColumn], SupColumn) =
    (to, from) match
      case (toCols, (fromCol, indices)) if indices.forall(_.isInstanceOf[IndexPath]) =>
        updateSupplementaryChecked { sup =>
          val fromColIndex = relationOps.columnIndex(sup, fromCol)
          to.zip(indices).foldLeft(sup) { case (acc, (col, IndexPath(path))) =>
            relationOps.map(acc, col) { row =>
              path.foldLeft(row(fromColIndex)) { (v, i) => tupleOps.project(v, i) }
            }
          }
        }
        (toCols, fromCol)
      case _ =>
        super.bindInSupplementary(to, from)

  override protected def evalAssignOpen(to: Term, from: Term)(using Fixed): (Seq[SupColumn], SupColumn) =
    val res = super.evalAssignOpen(to, from)
    to match
      case TupleLit(ts) if ts.exists(canDetermineValue) =>
        // Some variable in the tuple were already bound. Verify that the unpacking was valid.
        evalEq(to, from, false)
      case _ => // nothing
    res

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case TupleLit(ts) => naryOp(ts.map(evalTerm))(tupleOps.tupleLit)
    case Project(t, idx) => unaryOp(evalTerm(t))(tupleOps.project(_, idx))
    case _ => super.evalTermOpen(term)

