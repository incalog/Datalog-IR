package inca.ir.extension.string

import inca.ir.extension.arithmetic.TInt
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.{Atom, TAny, Term, TermType, Type}

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
    case Substring(t, index, length) =>
      (inferTerm(t, Mode.Bound).ty, inferTerm(index, Mode.Bound).ty, inferTerm(length, Mode.Bound).ty) match
        case (TString, TInt, TInt) => TString.bound
        case _ =>
          error(s"Ill-typed substring for term of type $t with index $index and length $length", term)
          TAny.bound
    case OrdinalNumber(t) =>
      inferTerm(t, Mode.Bound).ty match
        case TString => TInt.bound
        case _ =>
          error(s"Ill-typed ordinal number term for expected string of type $t", term)
          TInt.bound
    case StringLength(t) =>
      inferTerm(t, Mode.Bound).ty match
        case TString => TInt.bound
        case _ =>
          error(s"Ill-typed string length term for expected string of type $t", term)
          TInt.bound
    case _ => super.inferTermExtend(term, mode)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case RegexMatch(t, pattern, neg) =>
      (inferTerm(t, Mode.Bound).ty, inferTerm(pattern, Mode.Bound).ty) match
        case (TString, TString) => // ok
        case (ty1, ty2) =>
          error(s"Ill-typed regex match $atom with operand types $ty1 and $ty2", atom)
          TString.bound
    case _ =>
      super.checkAtom(atom, mode)

  override def checkType(ty: Type): Unit = ty match
    case TString => // good
    case _ => super.checkType(ty)

