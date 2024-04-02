package inca.ir.extension.arithmetic

import inca.ir.{Atom, Cast, Term}
import inca.ir.analysis.{VBool, Value}
import inca.ir.optimize.BaseIROptimizer

trait Optimizer extends BaseIROptimizer:
  override val name: String = "Arithmetic optimizer"
  
  override def visitTerm(term: Term): Seq[Term] =
    if (!term.typ.get.mode.isBinding)
      (term, termResult(term)) match
        // Preserve cast information
        case (Cast(t, ty), Some(Value.Int(i))) => Seq(Cast(IntNum(i), ty))
        case (Cast(t, ty), Some(Value.Double(d))) => Seq(Cast(DoubleNum(d), ty))
        case (_, Some(Value.Int(i))) => Seq(IntNum(i))
        case (_, Some(Value.Double(d))) => Seq(DoubleNum(d))
        case _ => super.visitTerm(term)
    else
      super.visitTerm(term)
