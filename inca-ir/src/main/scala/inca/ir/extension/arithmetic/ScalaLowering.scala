package inca.ir.extension.arithmetic

import inca.Scala
import inca.ir.Hint.preserveHints
import inca.ir.{Atom, BaseIR, Eq, Language, Name, Term, TermType, Type, Var}
import inca.ir.extension.*
import inca.ir.extension.bool.{BoolTrue, TBoolean}
import inca.ir.extension.arithmetic.IR
import inca.ir.extension.primitiveScala.{Application, Constant, TScala}
import inca.ir.lowering.BaseLowering


object ScalaLowering extends ScalaLowering
trait ScalaLowering extends BaseLowering:
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(primitiveScala.IR, block.IR)

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

  private def app(op: String, ty: TScala, lhsParam: (Term, Option[Type]), rhsParam: (Term, Option[Type])): (Var, Application) = {
    val x = Var(freshName())
    val (lhs, lhsTy) = lhsParam
    val (rhs, rhsTy) = rhsParam
    val calc = lamOp(op, "lhs" -> asScala(lhsTy), "rhs" -> asScala(rhsTy))
    val appl = Application(x, ty, calc, Seq(lhs, rhs))
    (x, appl)
  }

  private def blockApp(op: String, ty: TScala, lhs: (Term, Option[Type]), rhs: (Term, Option[Type])): Term = {
    val (x, appl) = app(op, ty, lhs, rhs)
    block.Block(Seq(appl), x)
  }

  private def typedParams(t: Term): Seq[(Term, Option[Type])] = {
    val ty = t.typ match
      case Some(TermType(ty,_)) => ty.flatten
      case None => Seq()

    for ((t, i) <- visitTerm(t).zipWithIndex)
      yield t -> ty.lift(i)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case LT(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) =>
        val (x, appl) = app("<", TScala.bool, l, r)
        // TODO: We get back a scala bool here...
        Eq(Constant(Scala.BoolLiteral(true), TScala.bool), x)
      }
    case GT(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) =>
        val (x, appl) = app(">", TScala.bool, l, r)
        // TODO: We get back a scala bool here...
        Eq(Constant(Scala.BoolLiteral(true), TScala.bool), x)
      }
    case _ =>
      super.visitAtom(atom))

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match {
    case IntNum(i) =>
      Seq(Constant(Scala.IntLiteral(i), TScala.int))
    case DoubleNum(d) =>
      Seq(Constant(Scala.DoubleLiteral(d), TScala.double))
    case Add(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) => blockApp("+", TScala.int, l, r) }
    case Sub(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) => blockApp("-", TScala.int, l, r) }
    case Mul(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) => blockApp("*", TScala.int, l, r) }
    case Div(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) => blockApp("/", TScala.int, l, r) }
    case _ =>
      super.visitTerm(term)
  })

  override def visitType(ty: Type): Type = preserveHints(ty)(ty match
    case TInt => TScala(Scala.TypeName("Int"))
    case TDouble => TScala(Scala.TypeName("Double"))
    case _ => super.visitType(ty))
