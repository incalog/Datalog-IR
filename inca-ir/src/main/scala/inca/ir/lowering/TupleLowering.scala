package inca.ir.lowering

import inca.ir.{Atom, BaseIR, Body, Param, TInt, Term, Type, Var, extensions}
import inca.ir.extensions.{Project, TTuple, Tuple, TupleIR}

import scala.collection.immutable.{AbstractSeq, LinearSeq}

case class TupleLowering[S <: TupleIR, T <: BaseIR](override val src: S, override val trg: T) extends Lowering[S, T](src, trg) {
  override def loweredIRs: Set[BaseIR] = Set(new TupleIR {})

  // TODO: Implement

  override def visitParam(param: Param): Seq[Param] =
    val tys = visitType(param.ty)
    tys.zipWithIndex.map { (ty, idx) =>
      // Note: This name should be reserved, we could use gensym here to generate a fresh name and store the mapping
      Param(param.name.byAppending("$_" + idx), ty)
    }

  override def visitType(ty: Type): Seq[Type] = ty match
    case TTuple(tys) => tys.flatMap(visitType)
    case _ => super.visitType(ty)

  override def visitTerm(term: Term): Seq[Term] = term match
    // TODO: not enough. I need to think more about all the different cases
    case Project(t, idx) => visitTerm(t) match
        case Seq(Var(name)) => Seq(Var(name.byAppending("$_" + idx)))
        case ts: Seq[Term] if idx <= ts.size => ???
        case ts: Seq[Term] if idx > ts.size => throw IllegalArgumentException(s"Projection index $idx out of bounds!")
        case _ => throw IllegalArgumentException(s"Can not project unknown term: $term")
    case Tuple(ts) => ts.flatMap(visitTerm)
    case _ => super.visitTerm(term)
}
