package inca.ir.extension.foreign

import inca.ir.Hint.preserveHints
import inca.ir.{Atom, ModuleEntry, Term, Type}
import inca.ir.visitors.BaseIRVisitor

trait Visitor extends BaseIRVisitor {
  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case _: ForeignModuleEntry => Seq(moduleEntry)
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) { atom match
    case _: ForeignAtom => Seq(atom)
    case _ => super.visitAtom(atom)
  }

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
    case ft: ForeignTerm => ft.visitor.visitTerm(term)
    case ConvertForeignIR(t, fty, irty) => visitTerm(t).map(ConvertForeignIR(_, visitType(fty), visitType(irty)))
    case ConvertIRForeign(t, irty, fty) => visitTerm(t).map((term: Term) => ConvertIRForeign(term, visitType(irty), visitType(fty)))
    case _ => super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) { ty match
    case _: ForeignType => ty
    case _ => super.visitType(ty)
  }
}
