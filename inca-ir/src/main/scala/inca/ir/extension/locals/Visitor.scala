package inca.ir.extension.locals

import inca.ir.{ Atom, Var }
import inca.ir.visitors.BaseIRVisitor

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case DeclVal(v, t) => visitTerm(v).zip(visitTerm(t)).flatMap { 
      case (nv: Var, nt) =>  Some(DeclVar(nv, nt))
      case _ => None
    } 
    case DeclVar(v, t) => visitTerm(v).zip(visitTerm(t)).flatMap {
      case (nv: Var, nt) =>  Some(DeclVar(nv, nt))
      case _ => None
    }
    case Assign(v, t) => visitTerm(v).zip(visitTerm(t)).flatMap {
      case (nv: Var, nt) =>  Some(DeclVar(nv, nt))
      case _ => None
    } 
