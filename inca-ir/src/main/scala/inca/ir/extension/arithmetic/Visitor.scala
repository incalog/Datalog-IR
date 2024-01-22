package inca.ir.extension.arithmetic

import inca.ir.extension.not
import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, Term, Type}

trait Visitor extends BaseIRVisitor with not.Visitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case BinCompare(lhs, rhs, op) => visitTerm(lhs).zip(visitTerm(rhs)).map((l,r) => BinCompare(l, r, op))
    case _ => super.visitAtom(atom))

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match
    case BinOp(lhs, rhs, op) => visitTerm(lhs).zip(visitTerm(rhs)).map((l,r) => BinOp(l, r, op))
    case UnOp(t, op) => visitTerm(t).map(tt => UnOp(tt, op))
    case IntNum(value) => Seq(IntNum(value))
    case DoubleNum(value) => Seq(DoubleNum(value))
    case _ => super.visitTerm(term))

  override def visitType(ty: Type): Type =  preserveHints(ty)(ty match
    case TInt => TInt
    case TDouble => TDouble
    case _ => super.visitType(ty))

  override def negateAtom(atom: Atom): Atom = atom match
    case BinCompare(lhs, rhs, op) => BinCompare(lhs, rhs, op match
      case "<" => ">="
      case "<=" => ">"
      case ">" => "<="
      case ">=" => "<"
    )
    case _ => super.negateAtom(atom)
