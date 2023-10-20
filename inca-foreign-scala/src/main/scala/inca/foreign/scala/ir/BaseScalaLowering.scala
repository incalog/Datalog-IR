package inca.foreign.scala.ir

import inca.ir.lowering.BaseLowering
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.{ScalaInca, ScalaTerm, ScalaType}
import inca.foreign.scala.syntax.Scala
import inca.ir.Hint.preserveHints
import inca.ir.{BaseIR, Name, Term, TermType, Type}

trait BaseScalaLowering extends primitive.Visitor with BaseLowering:
  override def requiredIRs: Set[BaseIR] = Set(primitive.IR)

  def supportedTypes: Seq[Type]

  private var freshCount = 0

  protected[ir] def freshName(): Name =
    val x = primitive.IR.name + "$" + freshCount
    freshCount += 1
    Name(x)

  protected[ir] def createScalaBinOp(op: String, ty: ScalaType, lhsParam: (Term, ScalaType), rhsParam: (Term, ScalaType)): ScalaTerm = {
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

  protected[ir] def createScalaUnOp(op: String, ty: ScalaType, param: (Term, ScalaType)): ScalaTerm = {
    val (arg, argTy) = param
    val lambda = Scala.Lam(
      Seq(Scala.Param("arg", argTy.ty)),
      Scala.AppUnary(Scala.Id("arg"), op)
    )
    ScalaTerm(lambda, ty, Seq(arg))
  }

  protected[ir] def compileType(ty: Type): ScalaType =
    if (supportedTypes.contains(ty))
      ScalaInca.compileType(ty)
    else
      throw IllegalArgumentException(s"Unexpected type: $ty")

  override def visitType(ty: Type): Type = preserveHints(ty) {
    if (supportedTypes.contains(ty))
      compileType(ty)
    else
      super.visitType(ty)  
  }
  
  protected[ir] def typedParams(t: Term): Seq[(Term, Type)] = {
    val ty = t.typ match
      case Some(TermType(ty, _)) => ty.flatten
      case None => throw new IllegalStateException(s"Untyped term $t")

    for ((t, i) <- visitTerm(t).zipWithIndex)
      yield t -> ty(i)
  }
