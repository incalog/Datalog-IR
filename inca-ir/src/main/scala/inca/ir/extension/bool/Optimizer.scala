package inca.ir.extension.bool

import inca.ir
import inca.ir.analysis.Value
import inca.ir.optimize.BaseIROptimizer
import inca.ir.{Atom, Cast, Term}


trait Optimizer extends BaseIROptimizer:
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

  // Syntactic version
  /*override def visitTerm(term: Term): Seq[Term] = term match
    case BoolOr(BoolTrue, _) => Seq(BoolTrue)
    case BoolOr(_, BoolTrue) => Seq(BoolTrue)
    case BoolOr(t, BoolFalse) => visitTerm(t)
    case BoolOr(BoolFalse, t) => visitTerm(t)
    case BoolAnd(BoolFalse, _) => Seq(BoolFalse)
    case BoolAnd(_, BoolFalse) => Seq(BoolFalse)
    case BoolAnd(t, BoolTrue) => visitTerm(t)
    case BoolAnd(BoolTrue, t) => visitTerm(t)
    case BoolNot(BoolNot(t)) => visitTerm(t)
    case _ => super.visitTerm(term)*/
