package inca.ir.extension.aggregate

import inca.ir.*

object IR extends IR {}

trait IR extends BaseIR:
  override val name: String = "Aggregate"
  override def language: Language = super.language + IR
  override def requires: Language = Language(IR)

// TODO: Should we support count aggregation like this:
//  R(x, y)
//  count R(#z, x, y) where #z is the OutputAggregateArg

case class AggregateColumnArg(t: Term) extends Arg:
  override def toString: String = s"#$t"
  override def vars: Seq[Var] = t.vars
  override def commonVars: Set[Var] = t.commonVars

// TODO: Support aggregation over Extensional Relation ?
case class Aggregate(rel: Ref[Relation], args: Seq[Arg], op: AggregationOperator) extends Atom:
  override def toString: String = s"aggregate($rel(${args.mkString(", ")}), $op)"

  override def vars: Seq[Var] = args.flatMap(_.vars)
  override def commonVars: Set[Var] = args.flatMap(_.commonVars).toSet

  def aggregationColumns: Seq[Int] = args.zipWithIndex.filter(_._1.isInstanceOf[AggregateColumnArg]).map(_._2)

  def mapAggregateColumn(f: Term => Arg): Seq[Arg] =
    args.map {
      case AggregateColumnArg(t) => f(t)
      case a => a
    }
    
object Aggregate:
  def apply(name: Name, args: Seq[Arg], op: AggregationOperator): Aggregate =
    new Aggregate(RefByName(name), args, op)

trait AggregationOperator:
  def resultType: Type
  def typecheck(in: Seq[Type]): Option[String]

trait AggregationOperatorBuiltIn extends AggregationOperator

trait AggregationOperatorUserDefined extends AggregationOperator
