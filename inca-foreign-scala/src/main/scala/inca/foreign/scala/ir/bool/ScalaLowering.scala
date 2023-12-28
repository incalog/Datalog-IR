package inca.foreign.scala.ir.bool

import inca.foreign.scala.ir.primitive.{ScalaConstantTerm, ScalaInca, ScalaMonoAggregationOperator, ScalaTerm, ScalaType, ScalaLowering as BaseScalaLowering, IR as scalaIR}
import inca.ir.{BaseIR, Term, Type, Name}
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.bool.{AtomAsBool, BoolAnd, BoolFalse, BoolNot, BoolOr, BoolTrue, TBoolean, IR as boolIR}
import inca.ir.extension.mono.{MonoAggregationOperator, NaiveSetMonoDefinition}

trait ScalaLowering extends BaseScalaLowering:
  override def name: String = "ScalaBooleanLowering"
  override def loweredIRs: Set[BaseIR] = Set(boolIR)
  override def requiredIRs: Set[BaseIR] = super.requiredIRs + scalaIR

  override def isTypeSupported(ty: Type): Boolean = ty match
    case TBoolean => true
    case _ => super.isTypeSupported(ty)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
    case BoolTrue => Seq(ScalaConstantTerm("true", ScalaType.bool))
    case BoolFalse => Seq(ScalaConstantTerm("false", ScalaType.bool))
    case BoolAnd(t1, t2) => Seq(ScalaTerm("(t1: Boolean, t2: Boolean) => t1 & t2", ScalaType.bool, Seq(t1, t2)))
    case BoolOr(t1, t2) => Seq(ScalaTerm("(t1: Boolean, t2: Boolean) => t1 | t2", ScalaType.bool, Seq(t1, t2)))
    case BoolNot(t) => Seq(ScalaTerm("(t: Boolean) => !t", ScalaType.bool, Seq(t)))
    case AtomAsBool(atom) => ???
    case _ => super.visitTerm(term)
  }

  override def visitAggregationOperator(op: AggregationOperator): AggregationOperator = op match
    case MonoAggregationOperator(NaiveSetMonoDefinition(TBoolean)) =>
      ScalaMonoAggregationOperator(Name("ScalaNaiveSetMono$Boolean"), ScalaType("Boolean"), ScalaType("Set[Boolean]"), initCode = "Set[Boolean]()", addCode = "(st: Set[Boolean], a: Boolean) => st + a")
    case _ => super.visitAggregationOperator(op)