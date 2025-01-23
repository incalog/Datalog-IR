package inca.ir.extension.aggregate.analysis.logger

import inca.ir.*
import inca.ir.extension.aggregate.Aggregate
import inca.ir.analysis.base.logger.BaseAnalysisAnnotator

trait AnalysisAnnotator[V, RV, TV] extends BaseAnalysisAnnotator[V, RV, TV]:
  override def updateAtomResult(at: Atom): Unit = at match
    case Aggregate(rel, args, op) =>
      args.flatMap(extractTermAndVarName).foreach { (term, varName) =>
        extractTermValue(varName).foreach(updateTermResult(term, _))
      }