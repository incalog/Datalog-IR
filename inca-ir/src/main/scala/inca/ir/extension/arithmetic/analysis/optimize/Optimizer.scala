package inca.ir.extension.arithmetic.analysis.optimize

import inca.ir.extension.arithmetic.{DoubleNum, IntNum}
import inca.ir.optimize.BaseIROptimizer
import inca.ir.{Atom, Cast, Term}
import sturdy.values.Topped

trait Optimizer extends BaseIROptimizer:
  override val name: String = "Arithmetic optimizer"

  override def visitTerm(term: Term): Seq[Term] = super.visitTerm(term)

/*if (!term.typ.get.mode.isBinding)
  (term, termResults(term)) match
    // TODO: handle TermResult
    // Preserve cast information
    case (Cast(t, ty), Some(IntV(Topped.Actual(i)))) => Seq(Cast(IntNum(i), ty))
    case (Cast(t, ty), Some(DoubleV(Topped.Actual(d)))) => Seq(Cast(DoubleNum(d), ty))
    case (_, Some(IntV(Topped.Actual(i)))) => Seq(IntNum(i))
    case (_, Some(DoubleV(Topped.Actual(d)))) => Seq(DoubleNum(d))
    case _ => super.visitTerm(term)
else
  super.visitTerm(term)*/
