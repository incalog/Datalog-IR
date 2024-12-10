package inca.ir.extension.data.analysis.logger

import inca.ir.*
import inca.ir.analysis.base.logger.BaseAnalysisAnnotator
import inca.ir.extension.data.Deconstruct

trait AnalysisAnnotator[V, RV, TV] extends BaseAnalysisAnnotator[V, RV, TV]:

  // Not all AST-term nodes are visited. Handle the missing cases explicitly in this method.
  override def storeAtomResult(at: Atom): Unit = at match
    case Deconstruct(t, caseRef, args, neg) =>
      args.flatMap(extractTermAndVarName).foreach { (term, varName) =>
        storeTermResult(term, extractTermValue(varName))
      }
    case _ => super.storeAtomResult(at)
