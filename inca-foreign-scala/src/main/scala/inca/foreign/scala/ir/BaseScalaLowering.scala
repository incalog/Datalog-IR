package inca.foreign.scala.ir

import inca.ir.lowering.BaseLowering
import inca.ir.{Var, name2string, string2name}
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.{ScalaAggregationAtom, ScalaInca, ScalaTerm, ScalaType}
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate
import inca.ir.{Atom, BaseIR, Name, Term, TermType, Type}

trait BaseScalaLowering extends primitive.Visitor with BaseLowering:
  override def requiredIRs: Set[BaseIR] = Set(primitive.IR)

  def isTypeSupported(ty: Type): Boolean

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

    for ((t, i) <- visitTerm(t).zipWithIndex)
      yield t -> ty(i)
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

  // Aggregation

  def visitAggregationOperator(op: aggregate.AggregationOperator, ty: Type): aggregate.AggregationOperator = op

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case aggregate.Aggregate(rel, args, op) =>
      // We only support a single aggregation column
      val Seq((aggTerm, aggColumnIndex)) = args.zipWithIndex.flatMap {
        case (aggregate.AggregateArg.AggregateColumn(t), i) => Some((t, i))
        case _ => None
      }

      val aggOutType = aggTerm.typ match
        case Some(TermType(ty, _)) => ty
        case _ => throw IllegalAccessException(s"Illegal aggregate term: $aggTerm of unknown type")
      val Seq(outTerm) = visitTerm(aggTerm)

      val wildcard = Var(gensym.freshName("_"))
      val relTerms = args.map(_.term).updated(aggColumnIndex, wildcard).flatMap(visitTerm)

      Seq(ScalaAggregationAtom(visitAggregationOperator(op, aggOutType), rel.name, outTerm, relTerms, aggColumnIndex))
    case _ =>
      super.visitAtom(atom)
