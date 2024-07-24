package inca.ir.extension.arithmetic

import inca.ir.extension.arithmetic.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.{Atom, TAny, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:
  private def inferInfixOpType(lhs: Term, rhs: Term, opTerm: SourceLocation): Type =
    (inferTerm(lhs, Mode.Bound).ty, inferTerm(rhs, Mode.Bound).ty) match
      case (TInt, TInt) => TInt
      case (TDouble, TDouble) => TDouble
      case (ty1@(TInt|TDouble), ty2) =>
        error(s"Expected $ty1 but got $ty2", rhs)
        ty1
      case (ty1, ty2@(TInt|TDouble)) =>
        error(s"Expected $ty2 but got $ty1", lhs)
        ty2
      case (ty1, ty2) =>
        error(s"Ill-typed operation $opTerm with operand types $ty1 and $ty2", opTerm)
        TInt

  private def inferUnOpType(t: Term, opTerm: SourceLocation): Type = inferTerm(t, Mode.Bound).ty match
    case TInt => TInt
    case TDouble => TDouble
    case ty =>
      error(s"Ill-typed operation $opTerm with operand type $ty", opTerm)
      TInt

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case BinOp(lhs, rhs, op) => inferInfixOpType(lhs, rhs, term).bound
    case UnOp(t, op) => inferUnOpType(t, term).bound
    case ToInt(t) =>
      inferTerm(t, Mode.Bound).ty
      TInt.bound
    case IntNum(_) => TInt.bound
    case DoubleNum(_) => TDouble.bound
    case _ => super.inferTermExtend(term, mode)

  protected override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case BinCompare(lhs, rhs, op) => inferInfixOpType(lhs, rhs, atom)
    case _ => super.checkAtom(atom, mode)

  override def checkType(ty: Type): Unit = ty match
    case TInt | TDouble => // good
    case _ => super.checkType(ty)
