package inca.ir.extension.arithmetic

import inca.Scala.{DoubleLiteral, IntLiteral}
import inca.ir.{Atom, Term}
import inca.ir.extensions.{Application, Constant, PrimitiveScalaIR}
import inca.ir.lowering.BaseLowering

trait Lowering[S <: IR, T <: PrimitiveScalaIR] extends BaseLowering[S, T] {
  override def visitTerm(term: Term): Seq[Term] = term match {
    case IntNum(i) => Seq(Constant(IntLiteral(i)))
    case DoubleNum(d) => Seq(Constant(DoubleLiteral(d)))
      //case Add(lh, rhs) => Application
    case _ => super.visitTerm(term)
  }
}
