package inca.foreign.scala.ir.arithmetic

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.{Atom, BaseIR, Eq, Term, TermType, Type, Var}
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{ArithmeticAggregationOperator, BinCompare, BinOp, DoubleNum, IntNum, TDouble, TInt, UnOp}
import inca.ir.extension.block
import inca.ir.extension.aggregate
import inca.ir.extension.mono.{ArithmeticMonoDefinition, MonoAggregationOperator}
import inca.ir.{name2string, string2name}
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.{ScalaAggregationAtom, ScalaAggregationOperator, ScalaConstantTerm, ScalaInca, ScalaTerm, ScalaType, ScalaLowering as BaseScalaLowering}
import inca.ir.util.SourceLocation

trait ScalaLowering extends BaseScalaLowering:
  override val loweredIRs: Set[BaseIR] = Set(arithmetic.IR)
  override val requiredIRs: Set[BaseIR] = super.requiredIRs ++ Set(block.IR)

  override def isTypeSupported(ty: Type): Boolean = ty match
    case TInt | TDouble  => true
    case _ => super.isTypeSupported(ty)

  override def visitAggregationOperator(op: aggregate.AggregationOperator, ty: Type): aggregate.AggregationOperator =
    op match
      case ArithmeticAggregationOperator.Count => ScalaAggregationOperator.Count
      case ArithmeticAggregationOperator.SumInt => ScalaAggregationOperator.Sum(compileType(ty))
      case ArithmeticAggregationOperator.MinInt => ScalaAggregationOperator.Min(compileType(ty))
      case ArithmeticAggregationOperator.MaxInt => ScalaAggregationOperator.Max(compileType(ty))
      case MonoAggregationOperator(ArithmeticMonoDefinition.Sum) => ScalaAggregationOperator.SumMono
      case MonoAggregationOperator(ArithmeticMonoDefinition.Max) => ScalaAggregationOperator.MaxMono
      case _ => op

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case BinCompare(lhs, rhs, op) =>
        typedParams(lhs).zip(typedParams(rhs)).map {
          case ((l, lty), (r, rty)) if lty == rty =>
            val ty = compileType(lty)
            Eq(ScalaConstantTerm.TRUE, createScalaBinOp(op, ScalaType.bool, l -> ty, r -> ty))
          case ((_, lty), (_, rty)) =>
            throw IllegalStateException(s"Can not apply `$op` to incompatible types: $lty and $rty")
        }
      case _ => super.visitAtom(atom)
  }

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case IntNum(i) =>
        Seq(ScalaConstantTerm(s"$i", ScalaType.int))
      case DoubleNum(d) =>
        Seq(ScalaConstantTerm(s"$d", ScalaType.double))
      case BinOp(lhs, rhs, op) =>
        typedParams(lhs).zip(typedParams(rhs)).map {
          case ((l, lty), (r, rty)) if lty == rty =>
            val ty = compileType(lty)
            val v = Var(freshName())
            block.Block(Eq(v, createScalaBinOp(op, ty, l -> ty, r -> ty)), v)
          case ((_, lty), (_, rty)) =>
            throw IllegalStateException(s"Can not apply `$op` to incompatible types: $lty and $rty")
        }
      case UnOp(term, op) =>
        typedParams(term).map { case (t, ty) =>
          val sty = compileType(ty)
          createScalaUnOp(op, sty, t -> sty)
        }
      case _ =>
        super.visitTerm(term)
  }
