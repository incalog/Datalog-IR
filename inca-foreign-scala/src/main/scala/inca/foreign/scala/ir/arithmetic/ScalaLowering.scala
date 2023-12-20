package inca.foreign.scala.ir.arithmetic

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.{Atom, BaseIR, Eq, Term, TermType, Type, Var}
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{ArithmeticAggregationOperator, BinCompare, BinOp, DoubleNum, IntNum, TDouble, TInt, UnOp}
import inca.ir.extension.block
import inca.ir.extension.aggregate
import inca.ir.extension.mono.{ArithmeticMonoDefinition, MonoAggregationOperator, MonoDefinition}
import inca.ir.{name2string, string2name}
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.ScalaMonoDefinition.builtinMono
import inca.foreign.scala.ir.primitive.{ScalaAggregationOperator, ScalaConstantTerm, ScalaInca, ScalaMonoAggregationOperator, ScalaMonoDefinition, ScalaType, ScalaLowering as BaseScalaLowering}

trait ScalaLowering extends BaseScalaLowering:
  override val name: String = "ScalaArithmetic"
  override val loweredIRs: Set[BaseIR] = Set(arithmetic.IR)
  override val requiredIRs: Set[BaseIR] = super.requiredIRs ++ Set(block.IR)

  override def isTypeSupported(ty: Type): Boolean = ty match
    case TInt | TDouble  => true
    case _ => super.isTypeSupported(ty)

  override def visitAggregationOperator(op: aggregate.AggregationOperator): aggregate.AggregationOperator =
    op match
      case ArithmeticAggregationOperator.Count =>
        ScalaAggregationOperator(
          name = "Count",
          ty = ScalaType.int,
          initCode = "0",
          addCode = "(x: Int, y: Any) => x + 1",
        )
      case ArithmeticAggregationOperator.SumInt =>
        ScalaAggregationOperator(
          name = "SumInt",
          ty = ScalaType.int,
          initCode = "0",
          addCode = "(x: Int, y: Int) => x + y",
        )
      case ArithmeticAggregationOperator.SumDouble =>
        ScalaAggregationOperator(
          name = "SumDouble",
          ty = ScalaType.double,
          initCode = "0",
          addCode = "(x: Double, y: Double) => x + y",
        )
      case ArithmeticAggregationOperator.MinInt =>
        ScalaAggregationOperator(
          name = "MinInt",
          ty = ScalaType.int,
          initCode = "Int.MaxValue",
          addCode = "(x: Int, y: Int) => x min y",
        )
      case ArithmeticAggregationOperator.MinDouble =>
        ScalaAggregationOperator(
          name = "MinDouble",
          ty = ScalaType.double,
          initCode = "Double.MaxValue",
          addCode = "(x: Double, y: Double) => x min y",
        )
      case ArithmeticAggregationOperator.MaxInt =>
        ScalaAggregationOperator(
          name = "MaxInt",
          ty = ScalaType.int,
          initCode = "Int.MinValue",
          addCode = "(x: Int, y: Int) => x max y",
        )
      case ArithmeticAggregationOperator.MaxDouble =>
        ScalaAggregationOperator(
          name = "MaxDouble",
          ty = ScalaType.double,
          initCode = "Double.MinValue",
          addCode = "(x: Double, y: Double) => x max y",
        )
      case MonoAggregationOperator(mono) =>
        val scalaMono = visitMonoDef(mono).asInstanceOf[ScalaMonoDefinition]
        ScalaMonoAggregationOperator(mono.name, ScalaInca.compileType(scalaMono.typ.in), ScalaInca.compileType(scalaMono.typ.state), scalaMono.initCode, scalaMono.addCode)
      case _ => super.visitAggregationOperator(op)

  override def visitMonoDef(mono: MonoDefinition): MonoDefinition = mono match
    case ArithmeticMonoDefinition.SumInt =>
      builtinMono(ArithmeticMonoDefinition.SumInt,
        initCode = "0",
        addCode = "(x:Int,y:Int) => x + y",
        resultCode = "(x: Int) => x"
      )
    case ArithmeticMonoDefinition.SumDouble =>
      builtinMono(ArithmeticMonoDefinition.SumInt,
        initCode = "0.0",
        addCode = "(x:Double,y:Double) => x + y",
        resultCode = "(x: Double) => x"
      )
    case ArithmeticMonoDefinition.MaxInt =>
      builtinMono(ArithmeticMonoDefinition.SumInt,
        initCode = "Int.MinValue",
        addCode = "(x:Int,y:Int) => x max y",
        resultCode = "(x: Int) => x"
      )
    case ArithmeticMonoDefinition.MaxDouble =>
      builtinMono(ArithmeticMonoDefinition.SumInt,
        initCode = "Double.NegativeInfinity",
        addCode = "(x:Double,y:Double) => x max y",
        resultCode = "(x: Double) => x"
      )
    case _ => super.visitMonoDef(mono)

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
