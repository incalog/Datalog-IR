package inca.foreign.scala.ir.primitive

import inca.ir.{Atom, BaseIR, Term, Type}
import inca.ir.visitors.BaseIRVisitor
import inca.ir.Hint.preserveHints

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor:
  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case ScalaTerm(closure, ty, args) => Seq(ScalaTerm(closure, ty, args.flatMap(visitTerm)))
      case _ => super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case ScalaType(sty) => ScalaType(sty)
      case _ => super.visitType(ty)
  }