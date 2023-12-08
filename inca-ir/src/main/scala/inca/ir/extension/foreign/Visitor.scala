package inca.ir.extension.foreign

import inca.ir.{Atom, ModuleEntry, Term, Type}
import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor

trait Visitor extends BaseIRVisitor {
  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry) { moduleEntry match
    case _: ForeignModuleEntry => Seq(moduleEntry)
    case _ => super.visitModuleEntry(moduleEntry)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) { atom match
    case _: ForeignAtom => Seq(atom)
    case _ => super.visitAtom(atom)
  }

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
    case _: ForeignTerm => Seq(term)
    case _ => super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) { ty match
    case _: ForeignType => ty
    case _ => super.visitType(ty)
  }
}
