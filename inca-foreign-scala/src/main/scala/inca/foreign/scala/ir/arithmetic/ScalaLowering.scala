package inca.foreign.scala.ir.arithmetic

import inca.ir.{Atom, BaseIR, Eq, ModuleEntry, Name, Relation, Term, TermType, Type, Var}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{BinCompare, BinOp, DoubleNum, IntNum, TDouble, TInt}
import inca.ir.extension.block
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.{ScalaInca, ScalaTerm, ScalaType}
import inca.foreign.scala.syntax.Scala
import inca.ir
import inca.ir.Hint.preserveHints

trait ScalaLowering extends primitive.Visitor with BaseLowering:
  override val loweredIRs: Set[BaseIR] = Set(arithmetic.IR)
  override val requiredIRs: Set[BaseIR] = Set(primitive.IR, block.IR)

  private var freshCount = 0
  def freshName(): Name =
    val x = primitive.IR.name + "$" + freshCount
    freshCount += 1
    Name(x)

  private def createScalaBinOp(op: String, ty: ScalaType, lhsParam: (Term, ScalaType), rhsParam: (Term, ScalaType)): ScalaTerm = {
    val (lhs, lhsTy) = lhsParam
    val (rhs, rhsTy) = rhsParam
    val lambda = Scala.Lam(
      Seq(
        Scala.Param("lhs", lhsTy.ty),
        Scala.Param("rhs", rhsTy.ty)
      ),
      Scala.AppInfix(
        Scala.Id("lhs"), op, Scala.Id("rhs")
      )
    )
    ScalaTerm(lambda, ty, Seq(lhs, rhs))
  }

  private def typedParams(t: Term): Seq[(Term, Type)] = {
    val ty = t.typ match
      case Some(TermType(ty,_)) => ty.flatten
      case None => throw new IllegalStateException(s"Untyped term $t")

    for ((t, i) <- visitTerm(t).zipWithIndex)
      yield t -> ty(i)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case BinCompare(lhs, rhs, op) =>
        val constTrue = ScalaTerm(Scala.BoolLiteral(true), ScalaType.bool, Seq())

        typedParams(lhs).zip(typedParams(rhs)).map {
          case ((l, TInt), (r, TInt)) =>
            Eq(constTrue, createScalaBinOp(op, ScalaType.bool, l -> ScalaType.int, r -> ScalaType.int))
          case ((l, TDouble), (r, TDouble)) =>
            Eq(constTrue, createScalaBinOp(op, ScalaType.bool, l -> ScalaType.double, r -> ScalaType.double))
          case ((l, lty), (r, rty)) =>
            throw IllegalStateException(s"Can not compare types $lty and $rty")
        }
      case _ =>
        super.visitAtom(atom)
  }

  private def assignmentBlock(lhs: Term, rhs: Term) = block.Block(Eq(lhs, rhs), lhs)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case IntNum(i) =>
        Seq(ScalaTerm(Scala.IntLiteral(i), ScalaType.int, Seq()))
      case DoubleNum(d) =>
        Seq(ScalaTerm(Scala.DoubleLiteral(d), ScalaType.double, Seq()))
      case BinOp(lhs, rhs, op) =>
        typedParams(lhs).zip(typedParams(rhs)).map {
          case ((l, TInt), (r, TInt)) =>
            assignmentBlock(
              Var(freshName()),
              createScalaBinOp(op, ScalaType.int, l -> ScalaType.int, r -> ScalaType.int)
            )
          case ((l, TDouble), (r, TDouble)) =>
            assignmentBlock(
              Var(freshName()),
              createScalaBinOp(op, ScalaType.double, l -> ScalaType.double, r -> ScalaType.double)
            )
          case _ =>
            throw IllegalStateException(s"Can not lower incompatible binary operation: $term")
        }
      case _ =>
        super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case TInt => ScalaType.int
      case TDouble => ScalaType.double
      case _ => super.visitType(ty)
  }
