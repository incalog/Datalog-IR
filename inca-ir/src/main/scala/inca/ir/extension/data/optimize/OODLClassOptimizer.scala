package inca.ir.extension.data.optimize

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.analysis.base.values.Value
import inca.ir.extension.data.analysis.interpreter.DataKindV
import inca.ir.extension.data.{Construct, Deconstruct}
import inca.ir.extension.datamatch.Match
import inca.ir.optimize.{OODLClassBaseIROptimizer, isFalse, isTrue}
import sturdy.values.{Top, Topped}

trait OODLClassOptimizer extends OODLClassBaseIROptimizer:

  override def visitAtom(atom: Atom): Seq[Atom] = super.visitAtom(atom) /*preserveHints(atom) {
    atom match
      case Deconstruct(t, caseRef, args, neg) => getDataKinds(t) match
        case Some(kinds) =>
          val possibleCaseDefs = kinds.caseDefs.map(_.name)
          if (!possibleCaseDefs.contains(caseRef.name))
            logOptimizationStat("failing deconstruct", 1, _+1)
            throw FailedBody
          super.visitAtom(atom)
        case _ => super.visitAtom(atom)
      case _ => super.visitAtom(atom)
  }*/

  


