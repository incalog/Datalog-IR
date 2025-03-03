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
      case SetLit(ts) => Seq(SetLit(ts.flatMap(visitTerm)))
      case SetFrom(ref) => Seq(SetFrom(visitRef(ref)))
      case SetIntersection(t1, t2) =>
        visitTerm(t1).zip(visitTerm(t2)).map(SetIntersection.apply)
      case SetUnion(ts) =>
        Seq(SetUnion(ts.flatMap(visitTerm)))
      case SetComprehension(elem, atoms) =>
        for (v <- visitTerm(elem)) yield
          SetComprehension(v, atoms.flatMap(visitAtom))
      case _ => super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case TSet(ty) => TSet(visitType(ty))
      case _ => super.visitType(ty)
  }