package inca.ir.extension.demand

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extensions.*
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case Demand(vs) => Seq(Demand(vs.flatMap(visitTerm)))
    case _ => super.visitAtom(atom))
