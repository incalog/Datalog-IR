package inca.ir.extension.tuple

import inca.ir.*
import inca.ir.extension.tuple.{IR, Project, TTuple, Tuple}
import inca.ir.lowering.BaseLowering

import scala.collection.immutable.{AbstractSeq, LinearSeq}

object Lowering:
  val separator: String = "_"

  def apply[S <: IR, T <: BaseIR](srcIR: S, trgIR: T): Lowering[S, T] = new Lowering[S, T] {
    override def src: S = srcIR
    override def trg: T = trgIR
  }

import Lowering.separator

trait Lowering[S <: IR, T <: BaseIR] extends BaseLowering[S, T] {

  override def loweredIRs: Set[BaseIR] = super.loweredIRs ++ Set(new IR {})

  private def flatten(name: Name, typ: Type): Seq[(Name, Type)] = typ match {
    case TTuple(tys) => tys.zipWithIndex.flatMap { case (ty, ix) =>
      flatten(Name(name.name + separator + ix), visitType(ty))
    }
    case _ => Seq((name, visitType(typ)))
  }

  private def flatten(param: Param): Seq[Param] =
    flatten(param.name, param.ty).map { case (n, t) => Param(n, t) }

  override def visitParam(param: Param): Seq[Param] = flatten(param)

  override def visitTerm(term: Term): Seq[Term] = term match
    case Project(t, idx) =>
      val tupleTy: TTuple = t.typ match {
        case Some(ty@TTuple(tys)) if idx <= tys.size =>
          ty
        case Some(ty@TTuple(tys)) if idx > tys.size =>
          throw IndexOutOfBoundsException(s"Projection index $idx out of bounds!")
        case Some(ty) =>
          throw IllegalStateException(s"Term $t has type ${ty}, but expected TTuple.")
        case None =>
          throw IllegalStateException(s"Untyped term $t")
      }
      val endIndex = idx + tupleTy.tys(idx).size
      visitTerm(t) match {
        case ts: Seq[Term] if endIndex <= ts.size =>
          ts.slice(idx, endIndex)
        case ts: Seq[Term] if endIndex > ts.size =>
          throw IndexOutOfBoundsException(s"Projection index range ($idx, $endIndex) out of bounds!")
        case _ =>
          throw IllegalStateException(s"Can not project unknown term: $term")
      }
    case Tuple(ts) =>
      ts.flatMap(visitTerm)
    case Var(name) =>
      val vars = flatten(name, term.typ.getOrElse(throw IllegalArgumentException(s"Untyped term $term")))
      vars.map { case (n, _) => Var(n) }
    case _ => super.visitTerm(term)
}
