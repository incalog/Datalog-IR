package inca.ir.extension.tuple.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.analysis.base.effect.BaseIRFailure
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.tuple.{Project, TupleLit}
import sturdy.data.{MayJoin, mapJoin, MakeJoined}

case object InvalidTupleProjection extends BaseIRFailure

trait TupleOps[V]:
  def tupleLit(ts: Seq[V]): V
  def project(t: V, index: Int): V
  def iter(t: V): Seq[V]

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val tupleOps: TupleOps[V]

  override protected def canDetermineValue(t: Term): Boolean = t match
    case TupleLit(ts) => ts.forall(canDetermineValue)
    case Project(t, idx) => canDetermineValue(t)
    case _ => super.canDetermineValue(t)

  // Transform a term or possible nested tuple into a flat structure with the corresponding index.
  // If the term is not a tuple, it has an empty indexPath.
  private def deconstructTupleTerm(term: ir.Term, indexPath: Seq[Int] = Seq()): Seq[(Term, Seq[Int])] = term match
    case TupleLit(ts) => ts.zipWithIndex.flatMap { (t, i) => deconstructTupleTerm(t, indexPath :+ i) }
    case _ => Seq(term -> indexPath)

  override protected def evalAssignOpen(to: ir.Term, from: ir.Term)(using Fixed): (Seq[SupColumn], SupColumn) =
    val resCol = evalTerm(from)
    val termsWithIndex = deconstructTupleTerm(to)
    val bindingColsWithIndex = termsWithIndex.collect {
      case (t, i) if !canDetermineValue(t) => extractVarName(t).get.name -> i
    }

    // bind all bindings terms. We need to have at least one, otherwise we wouldn't be in this method
    updateSupplementaryChecked { sup =>
      val resColIndex = relationOps.columnIndex(sup, resCol)
      bindingColsWithIndex.foldLeft(sup) { case (acc, (col, indexPath)) =>
        relationOps.map(acc, col) { row =>
          indexPath.foldLeft(row(resColIndex)) { (v, i) => tupleOps.project(v, i) }
        }
      }
    }

    // check if the assignment was valid, if it also contained bounded terms.
    // e.g. (1, x) = (1, 2)
    val assignmentWasPartiallyBound = bindingColsWithIndex.size < termsWithIndex.size
    if (assignmentWasPartiallyBound)
      evalEq(to, from, false)

    (bindingColsWithIndex.map(_._1), resCol)

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case TupleLit(ts) => naryOp(ts.map(evalTerm))(tupleOps.tupleLit)
    case Project(t, idx) => unaryOp(evalTerm(t))(tupleOps.project(_, idx))
    case _ => super.evalTermOpen(term)

