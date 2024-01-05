package inca.foreign.scala.ir.bool

import inca.foreign.scala.ir.primitive.{ScalaConstantTerm, ScalaInca, ScalaMonoAggregationOperator, ScalaTerm, ScalaType, IR as scalaIR, ScalaLowering as BaseScalaLowering}
import inca.ir.{BaseIR, Eq, Name, Term, Type, Var}
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.bool.{AtomAsBool, BoolAnd, BoolFalse, BoolNot, BoolOr, BoolTrue, TBoolean, IR as boolIR}
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.extension.block
import inca.ir.extension.mono.{MonoAggregationOperator, NaiveSetMonoDefinition}
import inca.ir.extension.not.WeakNot

trait ScalaLowering extends BaseScalaLowering:
  override def isTypeSupported(ty: Type): Boolean = ty match
    case TBoolean => true
    case _ => super.isTypeSupported(ty)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
    case BoolTrue => Seq(ScalaConstantTerm("true", ScalaType.bool))
    case BoolFalse => Seq(ScalaConstantTerm("false", ScalaType.bool))
    case BoolAnd(t1, t2) => Seq(ScalaTerm("(t1: Boolean, t2: Boolean) => t1 & t2", ScalaType.bool, Seq(t1, t2)))
    case BoolOr(t1, t2) => Seq(ScalaTerm("(t1: Boolean, t2: Boolean) => t1 | t2", ScalaType.bool, Seq(t1, t2)))
    case BoolNot(t) => Seq(ScalaTerm("(t: Boolean) => !t", ScalaType.bool, Seq(t)))
    case AtomAsBool(atom) => 
      val x = freshName()
      Seq(block.Block(Seq(Disjunction(Seq(
          DisjunctionAlternative(atom, Eq(Var(x), ScalaConstantTerm.TRUE)),
          DisjunctionAlternative(WeakNot(atom), Eq(Var(x), ScalaConstantTerm.FALSE))
        ))), Var(x)))
    case _ => super.visitTerm(term)
  }