package inca.ir.extension.aggregate.analysis.logger

import inca.ir.*
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.analysis.base.logger.BaseAnalysisAnnotator

trait AnalysisAnnotator[V, RV, TV] extends BaseAnalysisAnnotator[V, RV, TV]:
  override def extractTermAndVarName(arg: Arg): Option[(Term, String)] = arg match
    case AggregateColumnArg(t@Var(ref)) => Some((t, ref.name.name))
    case _ => super.extractTermAndVarName(arg)

  override def updateAtomResult(at: Atom): Unit = at match
    case Aggregate(rel, args, op) =>
      args.flatMap(extractTermAndVarName).foreach { (term, varName) =>
        if (term.typ.get.mode.isBinding)
          extractTermValue(varName).foreach(updateTermResult(term, _))
        else
          () // TODO: meet old and new term result to increase precision
      }
    case _ => super.updateAtomResult(at)