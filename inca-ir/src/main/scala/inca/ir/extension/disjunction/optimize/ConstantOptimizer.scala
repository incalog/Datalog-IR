package inca.ir.extension.disjunction.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import sturdy.values.{Top, Topped}
import inca.ir.*
import inca.ir.optimize.{ConstantBaseIROptimizer, isFalse, isTrue}

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Disjunction(alternatives) =>
      // Do not visit the body here, since we don't have a body result for it.
      // ConstantBaseIROptimizer interprets a missing body result as a failing body.
      Seq(Disjunction(alternatives.map { alt => 
        DisjunctionAlternative(Body(alt.body.atoms.flatMap(visitAtom)))
      }))
    case _ => super.visitAtom(atom)


