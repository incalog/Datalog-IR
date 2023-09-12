package inca.ir.extension.set

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case SetMember(t1, t2) => visitTerm(t1).zip(visitTerm(t2)).map { case (s1, s2) => SetMember(s1, s2) }
      case _ => super.visitAtom(atom)
  }

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case Set(ts) => Seq(Set(ts.flatMap(visitTerm)))
      case SetIntersection(t1, t2) => visitTerm(t1).zip(visitTerm(t2)).map { case (s1, s2) => SetIntersection(s1, s2) }
      case SetUnion(t1, t2) => visitTerm(t1).zip(visitTerm(t2)).map { case (s1, s2) => SetUnion(s1, s2) }
      case _ => super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case TSet(ty) => TSet(visitType(ty))
      case _ => super.visitType(ty)
  }