package inca.ir.extension.string

import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.{Term, TermType}

trait Typechecker extends BaseIRTypechecker:
  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case StringLit(_) => TString.bound
    case StringConcat(lhs, rhs) => (inferTerm(lhs, Mode.Bound).ty, inferTerm(rhs, Mode.Bound).ty) match
      case (TString, TString) => TString.bound
      case (ty1, ty2) =>
        error(s"Ill-typed string concatentation $term with operand types $ty1 and $ty2", term)
        TString.bound
    case _ => super.inferTermExtend(term, mode)

