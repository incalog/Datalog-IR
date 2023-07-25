package inca.ir.extension.arithmetic

import inca.Scala
import inca.ir.{Atom, BaseIR, Eq, Name, Term, Type, Var}
import inca.ir.extension.*
import inca.ir.extension.bool.BoolTrue
import inca.ir.extension.arithmetic.IR
import inca.ir.extension.primitiveScala.{Application, Constant, IR => ScalaIR}
import inca.ir.extension.primitiveScala
import inca.ir.lowering.BaseLowering


trait Lowering[S <: IR, T <: ScalaIR with block.IR] extends BaseLowering[S, T] {
  override def loweredIRs: Set[BaseIR] = super.loweredIRs ++ Set(IR)

  private var freshCount = 0
  def freshName(): Name =
    val x = IR.name + "$" + freshCount
    freshCount += 1
    Name(x)

  private def lamOp(op: String, lhs: (String, Option[Type]), rhs: (String, Option[Type])): Scala.Term =
    Scala.Lam(
      // TODO: Add type information
      Seq("lhs" -> None, "rhs" -> None),
      Scala.AppInfix(Scala.Id("lhs"), "+", Scala.Id("rhs"))
    )

  private def app(op: String, lhs: Term, rhs: Term): (Var, Application) = {
    val x = Var(freshName())
    // TODO: We need something like `asScala`. How do we handle this best so that it is extensible
    val appl = Application(x, lamOp("+", "lhs" -> None, "rhs" -> None), Seq(lhs, rhs))
    (x, appl)
  }

  private def blockApp(op: String, lhs: Term, rhs: Term): Term = {
    val (x, appl) = app(op, lhs, rhs)
    block.Block(Seq(appl), x)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case LT(lhs, rhs) =>
      visitTerm(lhs).zip(visitTerm(rhs)).map { case (l, r) =>
        val (x, appl) = app("<", l, r)
        // TODO: BoolTrue is probably not what we want, we get back a scala bool here...
        Eq(Constant(Scala.BoolLiteral(true)), x)
      }
    case GT(lhs, rhs) =>
      visitTerm(lhs).zip(visitTerm(rhs)).map { case (l, r) =>
        val (x, appl) = app(">", l, r)
        // TODO: BoolTrue is probably not what we want, we get back a scala bool here...
        Eq(Constant(Scala.BoolLiteral(true)), x)
      }
    case _ =>
      super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match {
    case IntNum(i) =>
      Seq(Constant(Scala.IntLiteral(i)))
    case DoubleNum(d) =>
      Seq(Constant(Scala.DoubleLiteral(d)))
    case Add(lhs, rhs) =>
      visitTerm(lhs).zip(visitTerm(rhs)).map { case (l, r) => blockApp("+", l, r)}
    case Sub(lhs, rhs) =>
      visitTerm(lhs).zip(visitTerm(rhs)).map { case (l, r) => blockApp("-", l, r)}
    case Mul(lhs, rhs) =>
      visitTerm(lhs).zip(visitTerm(rhs)).map { case (l, r) => blockApp("*", l, r)}
    case Div(lhs, rhs) =>
      visitTerm(lhs).zip(visitTerm(rhs)).map { case (l, r) => blockApp("/", l, r)}
    case _ =>
      super.visitTerm(term)
  }
}
