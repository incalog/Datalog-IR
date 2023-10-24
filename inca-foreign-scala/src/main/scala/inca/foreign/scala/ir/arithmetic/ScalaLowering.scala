package inca.foreign.scala.ir.arithmetic

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.{Atom, BaseIR, Eq, Term, TermType, Type, Var}
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{ArithmeticAggregationOperator, BinCompare, BinOp, DoubleNum, IntNum, TDouble, TInt, UnOp}
import inca.ir.extension.block
import inca.ir.extension.aggregate
import inca.ir.{name2string, string2name}
import inca.foreign.scala.ir.{BaseScalaLowering, primitive}
import inca.foreign.scala.ir.primitive.{ScalaAggregation, ScalaAggregationAtom, ScalaConstantTerm, ScalaInca, ScalaTerm, ScalaType}

trait ScalaLowering extends BaseScalaLowering:
  override val loweredIRs: Set[BaseIR] = Set(arithmetic.IR)
  override val requiredIRs: Set[BaseIR] = super.requiredIRs ++ Set(block.IR)

  override def isTypeSupported(ty: Type): Boolean = ty match
    case TInt | TDouble  => true
    case _ => false

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case BinCompare(lhs, rhs, op) =>
        typedParams(lhs).zip(typedParams(rhs)).map {
          case ((l, lty), (r, rty)) if lty == rty =>
            Eq(ScalaConstantTerm.TRUE, createScalaBinOp(op, ScalaType.bool, l -> compileType(lty), r -> compileType(rty)))
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
              case Some(TermType(ty, _)) => Some((compileType(ty), i))
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
