package inca.ir.extension.set

import inca.ir.Term
import inca.ir.visitors.IRVisitor

trait SyntacticOptimizer extends IRVisitor:
  override def name: String = "SyntacticSetOptimizer"

  private def distinguishAndSort(ts: Seq[Term]): Seq[Term] =
    ts.distinct.sorted { (a, b) => a.toString.compareTo(b.toString) }

  override def visitTerm(term: Term): Seq[Term] = term match
    case SetLit(ts) => Seq(SetLit(distinguishAndSort(ts.flatMap(visitTerm))))
    case SetUnion(ts) => Seq(SetUnion(distinguishAndSort(ts.flatMap(visitTerm))))
    case _ => super.visitTerm(term)