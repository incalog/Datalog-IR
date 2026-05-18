package inca.ir.extension.locals

import inca.ir.{ Atom, Var }
import inca.ir.visitors.BaseIRVisitor

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Assign(v, t) => visitTerm(v).zip(visitTerm(t)).flatMap {
      case (nv: Var, nt) => Some(Assign(nv, nt))
      case _ => None
    }
    case _ => super.visitAtom(atom)
