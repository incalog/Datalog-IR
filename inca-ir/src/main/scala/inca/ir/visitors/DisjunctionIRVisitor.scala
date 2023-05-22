package inca.ir.visitors

import inca.ir
import inca.ir.extensions.*
import inca.ir.*

import scala.collection.immutable.Seq

trait DisjunctionIRVisitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = atom match {
    case Disjunction(as1, as2) => Seq(Disjunction(as1.flatMap(visitAtom), as2.flatMap(visitAtom)))
    case _ => super.visitAtom(atom)
  }