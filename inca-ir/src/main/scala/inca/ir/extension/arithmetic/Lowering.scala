package inca.ir.extension.arithmetic

import inca.Scala
import inca.ir.{Atom, BaseIR, Eq, Name, Term, Type, Var}
import inca.ir.extension.*
import inca.ir.extension.bool.BoolTrue
import inca.ir.extension.arithmetic.IR
import inca.ir.extension.primitiveScala.{Application, Constant, TScala, IR as ScalaIR}
import inca.ir.extension.primitiveScala
import inca.ir.lowering.BaseLowering


trait Lowering[S <: IR, T <: ScalaIR with block.IR] extends BaseLowering[S, T] {
  override def loweredIRs: Set[BaseIR] = super.loweredIRs ++ Set(IR)

  private var freshCount = 0
  def freshName(): Name =
    val x = IR.name + "$" + freshCount
    freshCount += 1
    Name(x)

  // TODO: How do we handle this best so that it is extensible ?
  private def asScala(ty: Option[Type]): Option[Scala.Type] = ty match
    //case Some(TAny) => Some(Scala.TypeName("Any"))
    case Some(TInt) => Some(Scala.TypeName("Int"))
    case Some(TDouble) => Some(Scala.TypeName("Double"))
    case Some(TScala(ty)) => Some(ty)
    case None => None

  private def lamOp(op: String, lhs: (String, Option[Scala.Type]), rhs: (String, Option[Scala.Type])): Scala.Term =
    Scala.Lam(Seq(lhs, rhs), Scala.AppInfix(Scala.Id(lhs._1), op, Scala.Id(rhs._1)))

  private def app(op: String, lhsParam: (Term, Option[Type]), rhsParam: (Term, Option[Type])): (Var, Application) = {
    val x = Var(freshName())
    val (lhs, lhsTy) = lhsParam
    val (rhs, rhsTy) = rhsParam
    val calc = lamOp(op, "lhs" -> asScala(lhsTy), "rhs" -> asScala(rhsTy))
    val appl = Application(x, calc, Seq(lhs, rhs))
    (x, appl)
  }

  private def blockApp(op: String, lhs: (Term, Option[Type]), rhs: (Term, Option[Type])): Term = {
    val (x, appl) = app(op, lhs, rhs)
    block.Block(Seq(appl), x)
  }

  private def typedParams(t: Term): Seq[(Term, Option[Type])] = {
    val ty = t.typ match
      case Some(ty) => ty.flatten
      case None => Seq()

    for ((t, i) <- visitTerm(t).zipWithIndex)
      yield t -> ty.lift(i)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case LT(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) =>
        val (x, appl) = app("<", l, r)
        // TODO: We get back a scala bool here...
        Eq(Constant(Scala.BoolLiteral(true)), x)
      }
    case GT(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) =>
        val (x, appl) = app(">", l, r)
        // TODO: We get back a scala bool here...
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
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) => blockApp("+", l, r) }
    case Sub(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) => blockApp("-", l, r) }
    case Mul(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) => blockApp("*", l, r) }
    case Div(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) => blockApp("/", l, r) }
    case _ =>
      super.visitTerm(term)
  }
}
