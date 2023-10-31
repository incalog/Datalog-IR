package inca.ir.extension.impure

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case Impure(v, atoms, up, kind) =>
      val as = atoms.flatMap(visitAtom)
      visitTerm(up).map(t => Impure(v, as, t, kind))
    case _ => super.visitAtom(atom))
