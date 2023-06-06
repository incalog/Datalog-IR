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
