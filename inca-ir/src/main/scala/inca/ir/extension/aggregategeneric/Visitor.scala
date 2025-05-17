package inca.ir.extension.aggregategeneric

import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.ir.extension.not
import inca.ir.visitors.BaseIRVisitor
import inca.ir.*

trait Visitor extends BaseIRVisitor with not.Visitor:
  override def visitTerm(term: Term): Seq[Term] = term match
    case AggregateGeneric(Some(agg), ats, op) => visitTerm(agg).map { t =>
      AggregateGeneric(Some(t), ats.flatMap(visitAtom), op)
    }
    case AggregateGeneric(None, ats, op) =>
      Seq(AggregateGeneric(None, ats.flatMap(visitAtom), op))
    case _ => super.visitTerm(term)
