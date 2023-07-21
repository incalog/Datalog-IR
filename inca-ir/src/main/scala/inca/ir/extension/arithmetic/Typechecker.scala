package inca.ir.extension.arithmetic

import inca.ir.extension.arithmetic.*
import inca.ir.typing.BaseIRTypechecker
import inca.ir.{Atom, TAny, Term, Type}

trait Typechecker extends BaseIRTypechecker:
  override def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match
    case (TInt, TDouble) => false
    case (TDouble, TInt) => false
    case (TInt, TInt) => true
    case (TDouble, TDouble) => true
    case _ => super.subtype(ty1, ty2)

  override def typecheck(atom: Atom): Unit = atom match
    case LT(lhs, rhs) =>
      typecheck(lhs)
      typecheck(rhs)
    case GT(lhs, rhs) =>
      typecheck(lhs)
      typecheck(rhs)
    case _ => super.typecheck(atom)

  private def typecheckInfixOp(ty1: Type, ty2: Type) = {
    // TODO: We might implement better typechecking here
    join(ty1, ty2)
  }

  override def typecheckInternal(term: Term, inferred: Option[Type]): Type = term match
    case Add(lhs, rhs) => typecheckInfixOp(typecheck(lhs), typecheck(rhs))
    case Sub(lhs, rhs) => typecheckInfixOp(typecheck(lhs), typecheck(rhs))
    case Mul(lhs, rhs) => typecheckInfixOp(typecheck(lhs), typecheck(rhs))
    case Div(lhs, rhs) => typecheckInfixOp(typecheck(lhs), typecheck(rhs))
    case Min(lhs, rhs) => typecheckInfixOp(typecheck(lhs), typecheck(rhs))
    case Max(lhs, rhs) => typecheckInfixOp(typecheck(lhs), typecheck(rhs))
    case IntNum(_) => TInt
    case DoubleNum(_) => TDouble
    case _ => super.typecheckInternal(term, inferred)
