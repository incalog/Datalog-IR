package inca.ir.extension.arithmetic

import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, Term, Type}

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case LT(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(LT.apply)
    case GT(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(GT.apply)
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match
    case Add(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Add.apply)
    case Sub(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Sub.apply)
    case Mul(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Mul.apply)
    case Div(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Div.apply)
    case _ => super.visitTerm(term)

  override def visitType(ty: Type): Type = ty match
    case TInt => TInt
    case TDouble => TDouble
    case _ => super.visitType(ty)
