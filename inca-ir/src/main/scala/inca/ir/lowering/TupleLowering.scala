package inca.ir.lowering

import inca.ir.*
import inca.ir.extensions.{Project, TTuple, Tuple, TupleIR}

import scala.collection.immutable.{AbstractSeq, LinearSeq}

case class TupleLowering[S <: TupleIR, T <: BaseIR](override val src: S, override val trg: T) extends Lowering[S, T](src, trg) {
  override def loweredIRs: Set[BaseIR] = Set(new TupleIR {})

  private def flatten(name: Name, typ: Type): Seq[(Name, Type)] = typ match {
    case TTuple(tys) => tys.zipWithIndex.flatMap { case (ty, ix) =>
      flatten(Name(name.name + "_" + ix), visitType(ty))
    }
    case _ => Seq((name, visitType(typ)))
  }

  private def flatten(param: Param): Seq[Param] =
    flatten(param.name, param.ty).map { case (n, t) => Param(n, t) }

  private def size(ty: Type): Int = ty match {
    case TTuple(tys) => tys.map(size).sum
    case _ => 1
  }

  override def visitParam(param: Param): Seq[Param] = flatten(param)

  /*override def visitType(ty: Type): Seq[Type] = ty match
    case TTuple(tys) => tys.flatMap(visitType)
    case _ => super.visitType(ty)*/

  override def visitTerm(term: Term): Seq[Term] = term match
    case Project(t, idx) =>
      val tty: TTuple = t.typ match {
        case Some(ty@TTuple(tys)) if idx <= tys.size => ty
        case Some(ty@TTuple(tys)) if idx > tys.size => throw IllegalArgumentException(s"Projection index $idx out of bounds!")
        case Some(ty) => throw IllegalStateException(s"Term $t has type ${ty}, but expected TTuple.")
        case None => throw IllegalStateException(s"Untyped term $t")
      }
      val sizeOfProjectedElement = size(tty.tys(idx))
      visitTerm(t) match {
        case ts: Seq[Term] if idx + sizeOfProjectedElement <= ts.size => ts.slice(idx, idx + sizeOfProjectedElement)
        case ts: Seq[Term] if idx + sizeOfProjectedElement > ts.size => throw IllegalArgumentException(s"Projection index $idx out of bounds!")
        case _ => throw IllegalStateException(s"Can not project unknown term: $term")
      }
    case Tuple(ts) =>
      ts.flatMap(visitTerm)
    case Var(name) =>
      val vars = flatten(name, term.typ.getOrElse(throw IllegalArgumentException(s"Untyped expression $term")))
      vars.map { case (n, _) => Var(n) }
    case _ => super.visitTerm(term)
}
