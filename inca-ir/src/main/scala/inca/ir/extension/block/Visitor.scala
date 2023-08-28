package inca.ir.extension.block

import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, Term}

trait Visitor extends BaseIRVisitor:
  override def visitTerm(term: Term): Seq[Term] =  preserveHints(term)(term match
    case Block(as, t) =>
      for (v <- visitTerm(t))
        yield Block(as.flatMap(visitAtom), v)
    case _ => super.visitTerm(term))
