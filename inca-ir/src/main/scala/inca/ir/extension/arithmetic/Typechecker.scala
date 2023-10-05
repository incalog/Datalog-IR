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

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case Add(lhs, rhs) => inferInfixOpType(lhs, rhs, term).closed
    case Sub(lhs, rhs) => inferInfixOpType(lhs, rhs, term).closed
    case Mul(lhs, rhs) => inferInfixOpType(lhs, rhs, term).closed
    case Div(lhs, rhs) => inferInfixOpType(lhs, rhs, term).closed
    case Min(lhs, rhs) => inferInfixOpType(lhs, rhs, term).closed
    case Max(lhs, rhs) => inferInfixOpType(lhs, rhs, term).closed
    case Remainder(lhs, rhs) => inferInfixOpType(lhs, rhs, term).closed
    case IntNum(_) => TInt.closed
    case DoubleNum(_) => TDouble.closed
    case _ => super.inferTermExtend(term, mode)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case LT(lhs, rhs) => inferInfixOpType(lhs, rhs, atom)
    case GT(lhs, rhs) => inferInfixOpType(lhs, rhs, atom)
    case _ => super.checkAtom(atom, mode)

