package inca.ir.extension.tuple

import inca.ir.extension.tuple.{Project, TTuple, TupleLit}
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{TAny, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:
  override protected def join(ty1: Type, ty2: Type): Type = (ty1, ty2) match
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      TTuple(tys1.zip(tys2).map(join))
    case _ => super.join(ty1, ty2)

  override protected def meet(ty1: Type, ty2: Type): Type = (ty1, ty2) match
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      TTuple(tys1.zip(tys2).map(meet))
    case _ => super.meet(ty1, ty2)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match {
    case TupleLit(ts) =>
      val (tys,m)  = ts.foldRight((List.empty[Type],Mode.Bound)) { case (tt, (tys, m)) =>
        val TermType(tty, ttm) = inferTerm(tt, mode)
        (tty :: tys, m || ttm)
      }
      TermType(TTuple(tys), m)
    case Project(t, idx) => inferTerm(t, Mode.Bound).ty match {
      case TTuple(tys) if 0 <= idx && idx < tys.size => tys(idx).closed
      case TTuple(tys) =>
        error("Projection index out of bounds", term)
        TAny.closed
      case ty =>
        error(s"Expected tuple type but got $ty", term)
        TAny.closed
    }
    case _ => super.inferTermExtend(term, mode)
  }

