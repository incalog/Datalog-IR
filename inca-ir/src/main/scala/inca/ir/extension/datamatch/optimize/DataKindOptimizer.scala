package inca.ir.extension.datamatch.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.data.{Construct, Deconstruct}
import inca.ir.extension.data.analysis.interpreter.DataKindV
import sturdy.values.{Top, Topped}
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.datamatch.Match
import inca.ir.optimize.{DataKindBaseIROptimizer, isFalse, isTrue}

trait DataKindOptimizer extends DataKindBaseIROptimizer:

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case Match(matchee, cases) => getDataKinds(matchee) match
        case Some(kinds) =>
          val possibleCaseDefs = kinds.caseDefs.map(_.name)
          val filteredCases = cases.filter(c => possibleCaseDefs.contains(c.ref.name))

          val numUnreachableBranches = cases.size - filteredCases.size
          if (numUnreachableBranches != 0)
            logOptimizationStat("unreachable branches", 1, _+numUnreachableBranches)

          if (filteredCases.isEmpty)
            throw FailedBody
          Seq(Match(matchee, filteredCases))
        case _ => super.visitAtom(atom)
      case _ => super.visitAtom(atom)
  }

  


