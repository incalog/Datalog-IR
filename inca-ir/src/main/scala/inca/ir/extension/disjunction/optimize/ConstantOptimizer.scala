package inca.ir.extension.disjunction.optimize

import inca.ir
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.*
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Disjunction(alternatives) =>
      // Do not visit the body here, since we don't have a body result for it.
      // ConstantBaseIROptimizer interprets a missing body result as a failing body.
      Seq(Disjunction(alternatives.flatMap { alt =>
        try {
          val dis = DisjunctionAlternative(Body(alt.body.atoms.flatMap(visitAtom)))
          Some(dis)
        } catch { case FailedBody =>
          logOptimizationStat("constant failed disjunction body", 1, _+1)
          None
        }
      }))
    case _ => super.visitAtom(atom)


