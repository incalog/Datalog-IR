package inca.ir.extension.arithmetic

import inca.ir.{Atom, Cast, Term}
import inca.ir.analysis.{BaseIROptimizer, VBool, Value}

trait Optimizer extends BaseIROptimizer:
  override def visitTerm(term: Term): Seq[Term] =
    if (!term.typ.get.mode.isBinding)
      term match
        // Preserve cast information
        case Cast(t, ty) => termResult(term) match
          case Some(Value.Int(i)) => Seq(Cast(IntNum(i), ty))
          case Some(Value.Double(d)) => Seq(Cast(DoubleNum(d), ty))
          case _ => super.visitTerm(term)
        case _ => super.visitTerm(term)
    else
      super.visitTerm(term)
