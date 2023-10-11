package inca.ir.extension.arithmetic

import inca.ir.{Atom, Term}
import inca.ir.analysis.{BaseIROptimizer, Value, VBool}

trait Optimizer extends BaseIROptimizer:

  override def visitTerm(term: Term): Seq[Term] =
    if (!term.typ.get.mode.isBinding)
      termResult(term) match
        case Some(Value.Int(i)) => Seq(IntNum(i))
        case Some(Value.Double(d)) => Seq(DoubleNum(d))
        case _ => super.visitTerm(term)
    else
      super.visitTerm(term)
