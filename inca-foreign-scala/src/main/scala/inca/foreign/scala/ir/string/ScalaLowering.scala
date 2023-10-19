package inca.foreign.scala.ir.string

import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.{ScalaInca, ScalaTerm, ScalaType}
import inca.foreign.scala.syntax.Scala
import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.string.*
import inca.ir.extension.{string, block}
import inca.ir.lowering.BaseLowering
import inca.ir.*

// TODO: Refactor this with numeric lowering
trait ScalaLowering extends primitive.Visitor with BaseLowering:
  override val loweredIRs: Set[BaseIR] = Set(string.IR)
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

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case StringLit(value) =>
        Seq(ScalaTerm(Scala.StringLiteral(value), ScalaType.int, Seq()))
      case StringConcat(lhs, rhs) =>
        typedParams(lhs).zip(typedParams(rhs)).map {
          case ((l, TString), (r, TString)) =>
            createScalaBinOp("+", ScalaType.string, l -> ScalaType.string, r -> ScalaType.string)
          case ((l, lty), (r, rty)) =>
            throw IllegalStateException(s"Can not concat types $lty and $rty")
        }
      case _ =>
        super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case TString => ScalaType.string
      case _ => super.visitType(ty)
  }
