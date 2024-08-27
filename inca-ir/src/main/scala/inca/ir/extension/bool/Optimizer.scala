package inca.ir.extension.bool

import inca.ir
import inca.ir.optimize.BaseIROptimizer
import inca.ir.{Atom, Cast, Term}


/*trait Optimizer extends BaseIROptimizer:
  override val name: String = "Bool Optimizer"

  override def visitTerm(term: Term): Seq[Term] =
    if (!term.typ.get.mode.isBinding)
      (term, termResult(term)) match
        // Preserve cast information
        case (Cast(t, ty), Some(Value.Bool(true))) => Seq(Cast(BoolTrue, ty))
        case (Cast(t, ty), Some(Value.Bool(false))) => Seq(Cast(BoolFalse, ty))
        case (_, Some(Value.Bool(true))) => Seq(BoolTrue)
        case (_, Some(Value.Bool(false))) => Seq(BoolFalse)
        case _ => super.visitTerm(term)
    else
      super.visitTerm(term)
*/
