package inca.ir.typing
import inca.ir.extensions.{Project, TTuple, Tuple}
import inca.ir.{TAny, Term, Type}

trait TupleIRTypechecker extends BaseIRTypechecker:
  override protected[typing] def typecheckInternal(term: Term, inferred: Option[Type]): Type = term match {
    case Project(t, idx) => typecheck(t) match {
      case TTuple(tys) if idx < tys.size =>
        tys(idx)
      case TTuple(tys) =>
        error("Projection index out of bounds", t)
        TAny
      case ty =>
        error(s"Can not project on type: $ty", term)
        TAny
    }
    case Tuple(ts) => TTuple(ts.map(typecheck))
    case _ => super.typecheckInternal(term, inferred)
  }

  override protected[typing] def subtype(ty1: Type, ty2:  Type): Boolean = (ty1, ty2) match {
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      tys1.zip(tys2).forall { case (ty1, ty2)  => subtype(ty1, ty2) }
    case _ =>
      super.subtype(ty1, ty2)
  }

