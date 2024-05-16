package inca.ir.analysis

import inca.ir.Term
import inca.ir.extension.foreign.ForeignTerm

trait ForeignAbstractInterpreter[V, B] extends BaseAbstractInterpreter[V, B]:
  override def evalTermExtend(t: Term): TermResult = t match
    case _: ForeignTerm => TermResult(top, falseBool)
    case _ => super.evalTermExtend(t)
