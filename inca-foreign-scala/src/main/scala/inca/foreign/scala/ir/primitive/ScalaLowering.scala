package inca.foreign.scala.ir.primitive

import inca.foreign.scala.ir.{primitive, arithmetic as scalaArith, bool as scalaBool, data as scalaData, set as scalaSet, string as scalaString, tuple as scalaTuple}
import inca.foreign.scala.ir.primitive.{ScalaAggregationAtom, ScalaInca, ScalaTerm, ScalaType}
import inca.ir.{Arg, Atom, BaseIR, Name, TAny, Term, TermArg, TermType, Type, Var, WildcardArg, name2string, string2name}
import inca.ir.Hint.preserveHints
import inca.ir.extension.{aggregate, arithmetic, bool, set, string, tuple, data, demand, block}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.ir.extension.mono.{BuiltInMonoDefinition, MonoAggregationOperator, MonoDefinition, NaiveSetMonoDefinition, UserDefinedMonoDefinition}
import inca.util.Gensym

trait ScalaLowering extends BaseLowering with primitive.Visitor:
  override def name: String = "ScalaLowering"
  override def loweredIRs: Set[BaseIR] = Set(primitive.IR, bool.IR, string.IR, set.IR, data.IR, arithmetic.IR, tuple.IR)
  override def requiredIRs: Set[BaseIR] = Set(tuple.IR, demand.IR, set.IR, tuple.IR, block.IR)
  
  def isTypeSupported(ty: Type): Boolean = ty match
    case TAny => true
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

  /** Wildcards */

  override def visitArg(arg: Arg): Seq[Arg] = arg match
    case WildcardArg() => Seq(TermArg(Var(Name(gensym.freshName("_")))))
    case _ => super.visitArg(arg)

  /** Aggregation */
  // Note: Leave this in, otherwise we get a compiler error...
  override def visitAggregationOperator(op: aggregate.AggregationOperator): aggregate.AggregationOperator = op match
    // For user-defined mono definition
    case MonoAggregationOperator(ScalaMonoDefinition(name, initCode, addCode, resultCode, constructorParamTypes, typ)) =>
      ScalaMonoAggregationOperator(name, visitType(typ.in), visitType(typ.state), initCode, addCode)
    // TODO: How to deal with the mono definition that input and state have different type?
    case _ => super.visitAggregationOperator(op)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case aggregate.Aggregate(rel, args, op) =>
      // We only support a single aggregation column
      val Seq((aggTerm, aggColumnIndex)) = args.zipWithIndex.flatMap {
        case (AggregateColumnArg(t), i) => Some((t, i))
        case _ => None
      }

      val Seq(outTerm) = visitTerm(aggTerm)

      val argTerms = args.updated(aggColumnIndex, WildcardArg()).map {
        case AggregateColumnArg(t) => t
        case TermArg(t) => t
        case WildcardArg() => Var(gensym.freshName("_"))
      }.flatMap(visitTerm)
      Seq(ScalaAggregationAtom(visitAggregationOperator(op), rel.name, outTerm, argTerms, aggColumnIndex))
    case _ =>
      super.visitAtom(atom)


trait ForeignScalaLowering extends ScalaLowering 
  with scalaArith.ScalaLowering 
  with scalaBool.ScalaLowering
  with scalaData.ScalaLowering
  with scalaString.ScalaLowering
  with scalaTuple.ScalaLowering
  with scalaSet.ScalaLowering 
  