package inca.ir.extension.arithmetic

import inca.Scala.{DoubleLiteral, IntLiteral}
import inca.ir.{Atom, Name, Term}
import inca.ir.extension.*
import inca.ir.extensions.{Application, Constant, PrimitiveScalaIR}
import inca.ir.lowering.BaseLowering


trait Lowering[S <: IR, T <: PrimitiveScalaIR with block.IR] extends BaseLowering[S, T] {
  
  private var freshCount = 0
  def freshName(): Name =
    val x = IR.name + "$" + freshCount
    freshCount += 1
    Name(x)
  
  override def visitTerm(term: Term): Seq[Term] = term match {
    case IntNum(i) => Seq(Constant(IntLiteral(i)))
    case DoubleNum(d) => Seq(Constant(DoubleLiteral(d)))
    case Add(lh, rhs) =>
      val x = freshName()
      block.Block(
        Seq(
          Application(x, )
        ), 
      
      )
    case _ => super.visitTerm(term)
  }
}
