package inca.ir.extension.data.optimize

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.analysis.base.values.Value
import inca.ir.extension.data.analysis.interpreter.DataKindV
import inca.ir.extension.data.{Construct, Deconstruct}
import inca.ir.extension.datamatch.Match
import inca.ir.optimize.{DataKindBaseIROptimizer, isFalse, isTrue}
import sturdy.values.{Top, Topped}

trait DataKindOptimizer extends DataKindBaseIROptimizer:

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case Deconstruct(t, caseRef, args, neg) => getDataKinds(t) match
        case Some(kinds) =>
          val possibleCaseDefs = kinds.caseDefs.map(_.name)
          if (!possibleCaseDefs.contains(caseRef.name))
            println(s"Failing case: ${caseRef.name}")
            logOptimizationStat("failing deconstruct", 1, _+1)
            throw FailedBody
          super.visitAtom(atom)
        case _ => super.visitAtom(atom)
      case _ => super.visitAtom(atom)
  }

  


