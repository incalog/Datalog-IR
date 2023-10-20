package inca.foreign.scala.ir.arithmetic

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.{Atom, BaseIR, Eq, ModuleEntry, Name, Relation, Term, TermType, Type, Var}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{BinCompare, BinOp, UnOp, DoubleNum, IntNum, TDouble, TInt, ArithmeticAggregationOperator}
import inca.ir.extension.block
import inca.ir.extension.aggregate
import inca.ir.{name2string, string2name}

import inca.foreign.scala.ir.{BaseScalaLowering, primitive}
import inca.foreign.scala.ir.primitive.{ScalaInca, ScalaTerm, ScalaAggregationAtom, ScalaType, ScalaAggregation}
import inca.foreign.scala.syntax.Scala

trait ScalaLowering extends BaseScalaLowering:
  override val loweredIRs: Set[BaseIR] = Set(arithmetic.IR)
  override val requiredIRs: Set[BaseIR] = super.requiredIRs ++ Set(block.IR)

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
      case aggregate.Aggregate(rel, args, op: ArithmeticAggregationOperator) =>
        val agg = op match
          case ArithmeticAggregationOperator.Count => ScalaAggregation.Count
          case ArithmeticAggregationOperator.Sum => ScalaAggregation.Sum
          case ArithmeticAggregationOperator.Min => ScalaAggregation.Min
          case ArithmeticAggregationOperator.Max => ScalaAggregation.Max
        val aggTerms = args.map {
          case aggregate.AggregateArg.Arg(t) => t
          case aggregate.AggregateArg.AggregateColumn(t) => t match
            case Var(name) =>
              // create a fresh variable for the aggregated column
              Var(gensym.fresh(name))
            case _ => t
        }
        val aggInfo = args.zipWithIndex.flatMap {
          case (aggregate.AggregateArg.AggregateColumn(t), i) =>
            t.typ match
              case Some(TermType(TInt, _)) => Some((ScalaType.int, i))
              case Some(TermType(TDouble, _)) => Some((ScalaType.double, i))
              case Some(ty) => throw IllegalAccessException(s"Illegal aggregate term: $t of type: $ty")
              case _ => throw IllegalAccessException(s"Illegal aggregate term: $t of unknown type")
          case _ => None
        }.headOption
        val (ty, aggregateColumnIndex) = aggInfo.getOrElse(throw IllegalStateException("Missing aggregation info"))

        // TODO: Support Aggregations over tuples
        val outTerm = args.flatMap {
          case aggregate.AggregateArg.AggregateColumn(t) => visitTerm(t).headOption
          case _ => None
        }.head
        Seq(ScalaAggregationAtom(agg, rel.name, outTerm, ty, aggTerms.flatMap(visitTerm), aggregateColumnIndex))
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
      case UnOp(term, op) =>
        typedParams(term).map {
          case (t, TInt) =>
            createScalaUnOp(op, ScalaType.int, t -> ScalaType.int)
          case (t, TDouble) =>
            createScalaUnOp(op, ScalaType.double, t -> ScalaType.double)
          case (_, ty) =>
            throw IllegalStateException(s"Can not apply unary operation $op to: $term of type $ty")
        }
      case _ =>
        super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case TInt | TDouble => ScalaInca.compileType(ty)
      case _ => super.visitType(ty)
  }
