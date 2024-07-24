package inca.ir.extension.string

import inca.ir.extension.arithmetic.TInt
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.{Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:
  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case StringLit(_) => TString.bound
    case StringConcat(lhs, rhs) => (inferTerm(lhs, Mode.Bound).ty, inferTerm(rhs, Mode.Bound).ty) match
      case (TString, TString) => TString.bound
      case (ty1, ty2) =>
        error(s"Ill-typed string concatentation $term with operand types $ty1 and $ty2", term)
        TString.bound
    case ToString(t) =>
      inferTerm(t, Mode.Bound).ty
      TString.bound
    case Length(t) =>
      inferTerm(t, Mode.Bound).ty
      TInt.bound
    case SubstringFrom(t, index) =>
      (inferTerm(t, Mode.Bound).ty, inferTerm(index, Mode.Bound).ty) match
        case (TString, TInt) => TString.bound
        case (ty1, ty2) =>
          error(s"Ill-typed substring of $term with operand types $ty1 and $ty2", term)
          TString.bound
    case SubstringFromTo(t, start, end) =>
      (inferTerm(t, Mode.Bound).ty, inferTerm(start, Mode.Bound).ty, inferTerm(end, Mode.Bound).ty) match
        case (TString, TInt, TInt) => TString.bound
        case (ty1, ty2, ty3) =>
          error(s"Ill-typed substring of $term with operand types $ty1, $ty2 and $ty3", term)
          TString.bound
    case LastIndexOf(t, sub) => (inferTerm(t, Mode.Bound).ty, inferTerm(sub, Mode.Bound).ty) match
      case (TString, TString) => TString.bound
      case (ty1, ty2) =>
        error(s"Ill-typed lastIndexOf $term with operand types $ty1 and $ty2", term)
        TInt.bound
    case _ => super.inferTermExtend(term, mode)

  override def checkType(ty: Type): Unit = ty match
    case TString => // good
    case _ => super.checkType(ty)

