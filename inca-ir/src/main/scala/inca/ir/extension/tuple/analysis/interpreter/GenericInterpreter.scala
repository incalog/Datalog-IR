package inca.ir.extension.tuple.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.analysis.base.effect.{BaseIRFailure, InvalidBindings}
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, BindingInfo, Index, IndexPath, SupColumn}
import inca.ir.extension.tuple.{Project, TupleLit}
import sturdy.data.{MakeJoined, MayJoin, mapJoin}
import sturdy.values.Topped

case object InvalidTupleProjection extends BaseIRFailure

trait TupleOps[V]:
  def tupleLit(ts: Seq[V]): V
  def project(t: V, index: Int): V
  def iter(t: V): Seq[V]

case class TupleIndex(i: Int) extends Index

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val tupleOps: TupleOps[V]

  override protected def canDetermineValue(t: Term): Boolean = t match
    case TupleLit(ts) => ts.forall(canDetermineValue)
    case Project(t, idx) => canDetermineValue(t)
    case _ => super.canDetermineValue(t)

  override protected def extractBindingInfo(term: ir.Term, indexPath: IndexPath = Seq())(using rec: Fixed): Seq[BindingInfo] =
    term match
      case TupleLit(ts) =>
        val eleInfo = ts.zipWithIndex.flatMap { (t, i) => extractBindingInfo(t, indexPath :+ TupleIndex(i)) }
        if (canDetermineValue(term))
          val sup = evalTerm(term)
          BindingInfo(sup, indexPath, true) +: eleInfo
        else
          eleInfo
      case _ =>
        super.extractBindingInfo(term, indexPath)

  override protected def resolveNestedAtIndex(v: V, index: Index): V = index match
    case TupleIndex(i) => tupleOps.project(v, i)
    case _ => super.resolveNestedAtIndex(v, index)

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case TupleLit(ts) => naryOp(ts.map(evalTerm))(tupleOps.tupleLit)
    case Project(t, idx) => unaryOp(evalTerm(t))(tupleOps.project(_, idx))
    case _ => super.evalTermOpen(term)

