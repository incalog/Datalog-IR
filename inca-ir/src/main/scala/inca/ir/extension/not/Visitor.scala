package inca.ir.extension.not

import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, Term}

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case Not(at) =>
        visitAtom(at).map(Not.apply)
      case _ => super.visitAtom(atom)
  }
