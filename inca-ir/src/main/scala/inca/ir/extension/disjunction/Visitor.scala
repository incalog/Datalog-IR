package inca.ir.extension.disjunction

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case Disjunction(alts) => Seq(Disjunction(
      alts.flatMap(alt => visitBody(alt.body).map(DisjunctionAlternative.apply))
    ))
    case _ => super.visitAtom(atom))
