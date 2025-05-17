package inca.ir.extension.aggregategeneric

import inca.ir.*
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.ir.extension.arithmetic
import inca.ir.extension.set.TSet
import inca.ir.typing.{BaseIRTypechecker, DependencyInfo, Mode}

trait Typechecker extends BaseIRTypechecker:
  override protected def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case AggregateGeneric(agg, ats, op) =>
      scopedTypeContext {
        ats.foreach(checkAtom(_, Mode.Binding))
        if (agg.isDefined) {
          val aggTy = inferTerm(agg.get, Mode.Bound)
          op.typecheck(Seq(aggTy.ty))
        } else {
          if (op != arithmetic.ArithmeticAggregationOperator.Count)
            error("Can only handle count aggregation if no aggregation term is provided!")
        }
        op.resultType.bound
      }
    case _ => super.inferTermExtend(term, mode)

