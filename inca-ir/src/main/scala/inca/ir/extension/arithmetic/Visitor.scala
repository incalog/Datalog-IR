package inca.ir.extension.arithmetic

import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, Term, Type}

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] =  preserveHints(atom)(atom match
    case LT(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(LT.apply)
    case GT(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(GT.apply)
    case _ => super.visitAtom(atom))

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match
    case Add(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Add.apply)
    case Sub(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Sub.apply)
    case Mul(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Mul.apply)
    case Div(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Div.apply)
    case Remainder(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Remainder.apply)
    case Min(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Min.apply)
    case Max(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Max.apply)
    case Abs(lhs, rhs) => visitTerm(lhs).zip(visitTerm(rhs)).map(Abs.apply)
    case IntNum(value) => Seq(IntNum(value))
    case DoubleNum(value) => Seq(DoubleNum(value))
    case _ => super.visitTerm(term))

  override def visitType(ty: Type): Type =  preserveHints(ty)(ty match
    case TInt => TInt
    case TDouble => TDouble
    case _ => super.visitType(ty))
