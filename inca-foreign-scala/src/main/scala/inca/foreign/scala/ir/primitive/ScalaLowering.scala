package inca.foreign.scala.ir.primitive

import inca.foreign.scala.ir.{primitive, arithmetic as scalaArith, data as scalaData, string as scalaString}
import inca.ir.Hint.preserveHints
import inca.ir.extension.*
import inca.ir.lowering.BaseLowering
import inca.ir.*
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.edbdata.{TEdbList, TEdbNode, TEdbValue}
import inca.ir.extension.foreign.ForeignAggregationOperator

trait ScalaLowering extends BaseLowering with primitive.Visitor:
  override def name: String = "ScalaLowering"
  override def loweredIRs: Set[BaseIR] = Set(primitive.IR, bool.IR, string.IR, set.IR, data.IR, arithmetic.IR, tuple.IR)
  override def requiredIRs: Set[BaseIR] = Set(tuple.IR, demand.IR, set.IR, tuple.IR, block.IR, not.IR, disjunction.IR)
  
  def isTypeSupported(ty: Type): Boolean = ty match
    case TAny => true
    case TNothing => true
    case _ => false

  private var freshCount = 0

  protected[ir] def freshName(): Name =
    val x = primitive.IR.name + "$" + freshCount
    freshCount += 1
    Name(x)

  protected[ir] def createScalaBinOp(op: String, ty: ScalaType, lhsParam: (Term, ScalaType), rhsParam: (Term, ScalaType)): ScalaTerm = {
    val (lhs, lhsTy) = lhsParam
    val (rhs, rhsTy) = rhsParam
    val lambdaCode = s"(lhs: ${lhsTy.name}, rhs: ${rhsTy.name}) => lhs $op rhs"
    ScalaTerm(lambdaCode, ty, Seq(lhs, rhs))
  }

  protected[ir] def createScalaUnOp(op: String, ty: ScalaType, param: (Term, ScalaType)): ScalaTerm = {
    val (arg, argTy) = param
    val lambdaCode = s"(arg: $argTy) => ${op}arg"
    ScalaTerm(lambdaCode, ty, Seq(arg))
  }

  protected[ir] def typedParams(t: Term): Seq[(Term, Type)] = {
    val ty = t.typ match
      case Some(TermType(ty, _)) => ty.flatten
      case None => throw new IllegalStateException(s"Untyped term $t")

    for ((vt, i) <- visitTerm(t).zipWithIndex)
      yield vt -> ty(i)
  }
  
  /*
     Lower the type if it is supported by the IR, otherwise throw an error.
   */
  protected[ir] def compileType(ty: Type): ScalaType =
    if (isTypeSupported(ty))
      ScalaInca.compileType(ty)
    else
      throw IllegalArgumentException(s"Unexpected type: $ty")

  /*
  Lower the type if it is supported by the IR, otherwise call super.
   */
  override def visitType(ty: Type): Type = preserveHints(ty) {
    if (isTypeSupported(ty))
      compileType(ty)
    else
      super.visitType(ty)  
  }

  protected def createRelName(name: String): Name =
    gensym.freshName(
      Seq("(", ")", "[", "]", ", ").foldLeft(name)((s, t) => s.replace(t, "$"))
    )

trait ForeignScalaLowering extends ScalaLowering 
  with scalaArith.ScalaLowering 
  with scalaData.ScalaLowering
  with scalaString.ScalaLowering
