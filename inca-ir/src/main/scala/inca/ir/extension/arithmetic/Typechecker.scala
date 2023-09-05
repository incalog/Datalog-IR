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
      typecheckMust(lhs, None)
      typecheckMust(rhs, None)
    case GT(lhs, rhs) =>
      typecheckMust(lhs, None)
      typecheckMust(rhs, None)
    case _ => super.typecheck(atom)

  private def typecheckInfixOp(ty1: Type, ty2: Type): Type = {
    // TODO: We might implement better typechecking here
    join(ty1, ty2)
  }

  override def typecheckInternal(term: Term, hint: Option[Type]): Type = term match
    case Add(lhs, rhs) => typecheckInfixOp(typecheckMust(lhs, None), typecheckMust(rhs, None))
    case Sub(lhs, rhs) => typecheckInfixOp(typecheckMust(lhs, None), typecheckMust(rhs, None))
    case Mul(lhs, rhs) => typecheckInfixOp(typecheckMust(lhs, None), typecheckMust(rhs, None))
    case Div(lhs, rhs) => typecheckInfixOp(typecheckMust(lhs, None), typecheckMust(rhs, None))
    case Min(lhs, rhs) => typecheckInfixOp(typecheckMust(lhs, None), typecheckMust(rhs, None))
    case Max(lhs, rhs) => typecheckInfixOp(typecheckMust(lhs, None), typecheckMust(rhs, None))
    case IntNum(_) => TInt
    case DoubleNum(_) => TDouble
    case _ => super.typecheckInternal(term, hint)
