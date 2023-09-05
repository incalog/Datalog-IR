package inca.ir.extension.arithmetic

import inca.ir.extension.arithmetic.*
import inca.ir.typing.BaseIRTypechecker
import inca.ir.{Atom, TAny, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:
  override def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match
    case (TInt, TDouble) => false
    case (TDouble, TInt) => false
    case (TInt, TInt) => true
    case (TDouble, TDouble) => true
    case _ => super.subtype(ty1, ty2)

  override def typecheck(atom: Atom): Unit = atom match
    case LT(lhs, rhs) =>
      typecheck(lhs, None, Boundedness.Must)
      typecheck(rhs, None, Boundedness.Must)
    case GT(lhs, rhs) =>
      typecheck(lhs, None, Boundedness.Must)
      typecheck(rhs, None, Boundedness.Must)
    case _ => super.typecheck(atom)

  private def typecheckInfixOp(ty1: Type, ty2: Type): Type = {
    // TODO: We might implement better typechecking here
    join(ty1, ty2)
  }

  override def typecheckInternal(term: Term, hint: Option[Type], bound: Boundedness): Type = term match
    case Add(lhs, rhs) => typecheckInfixOp(typecheck(lhs, None, Boundedness.Must), typecheck(rhs, None, Boundedness.Must))
    case Sub(lhs, rhs) => typecheckInfixOp(typecheck(lhs, None, Boundedness.Must), typecheck(rhs, None, Boundedness.Must))
    case Mul(lhs, rhs) => typecheckInfixOp(typecheck(lhs, None, Boundedness.Must), typecheck(rhs, None, Boundedness.Must))
    case Div(lhs, rhs) => typecheckInfixOp(typecheck(lhs, None, Boundedness.Must), typecheck(rhs, None, Boundedness.Must))
    case Min(lhs, rhs) => typecheckInfixOp(typecheck(lhs, None, Boundedness.Must), typecheck(rhs, None, Boundedness.Must))
    case Max(lhs, rhs) => typecheckInfixOp(typecheck(lhs, None, Boundedness.Must), typecheck(rhs, None, Boundedness.Must))
    case IntNum(_) => TInt
    case DoubleNum(_) => TDouble
    case _ => super.typecheckInternal(term, hint, bound)
