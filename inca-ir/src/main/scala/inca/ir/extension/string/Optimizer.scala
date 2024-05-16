package inca.ir.extension.string

import inca.ir.analysis.{VBool, Value}
import inca.ir.optimize.BaseIROptimizer
import inca.ir.{Atom, Cast, Term}

trait Optimizer extends BaseIROptimizer:
  override val name: String = "String optimizer"

  override def visitTerm(term: Term): Seq[Term] =
    if (!term.typ.get.mode.isBinding)
      (term, termResult(term)) match
        // Preserve cast information
        case (Cast(t, ty), Some(Value.String(s))) => Seq(Cast(StringLit(s), ty))
        case (_, Some(Value.String(s))) => Seq(StringLit(s))
        case _ => super.visitTerm(term)
    else
      super.visitTerm(term)
