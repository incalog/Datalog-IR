package inca.ir.extension.bool

import inca.ir.analysis.{VBool, Value}
import inca.ir.optimize.BaseIROptimizer
import inca.ir.visitors.IRVisitor
import inca.ir.{Atom, Term}

// Simple syntactic optimizer
trait Optimizer extends IRVisitor:
  override val name: String = "SyntacticBoolOptimizer"

  override def visitTerm(term: Term): Seq[Term] = term match
    case BoolOr(BoolTrue, _) => Seq(BoolTrue)
    case BoolOr(_, BoolTrue) => Seq(BoolTrue)
    case BoolOr(t, BoolFalse) => visitTerm(t)
    case BoolOr(BoolFalse, t) => visitTerm(t)
    case BoolAnd(BoolFalse, _) => Seq(BoolFalse)
    case BoolAnd(_, BoolFalse) => Seq(BoolFalse)
    case BoolAnd(t, BoolTrue) => visitTerm(t)
    case BoolAnd(BoolTrue, t) => visitTerm(t)
    case BoolNot(BoolNot(t)) => visitTerm(t)
    case _ => super.visitTerm(term)
