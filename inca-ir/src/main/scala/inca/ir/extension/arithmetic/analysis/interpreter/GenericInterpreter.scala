package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir
import inca.ir.Atom
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.arithmetic.*
import sturdy.data.MayJoin
import sturdy.values.floating.FloatOps
import sturdy.values.integer.IntegerOps
import sturdy.values.ordering.OrderingOps

enum BinaryArithmeticComparisonOperator(raw: String):
  case Leq extends BinaryArithmeticComparisonOperator("<=")
  case Lt extends BinaryArithmeticComparisonOperator("<")
  case Gt extends BinaryArithmeticComparisonOperator(">")
  case Geq extends BinaryArithmeticComparisonOperator(">=")

object BinaryArithmeticComparisonOperator:
  def fromString(op: String): BinaryArithmeticComparisonOperator = op match
    case "<=" => Leq
    case "<" => Lt
    case ">" => Gt
    case ">=" => Geq
    case _ => throw IllegalStateException(s"Unexpected binary operator $op")

// Used to optimistically refine values under the assumption that the binary operation holds.
// That is, given two (abstract) values `v1` and `v2` and a binary comparison `v1 op v2`,
// this method returns refined versions of both values that exclude values where the operation cannot possibly be true.
//
// E.g.
//  Consider an interval analysis, given v1 = [10, 20], v2 = [5, 15], and op <.
//  We can refine to v1 = [10, 14], v2 = [11, 15]
//
// This is useful in abstract domains where value domains are narrowed based on relational conditions.
trait ArithmeticRefinementOps[V]:
  def refine(v1: V, v2: V, op: BinaryArithmeticComparisonOperator): (V, V)

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val intOps: IntegerOps[Int, V]
  val doubleOps: FloatOps[Double, V]
  val intOrderingOps: OrderingOps[V, B]
  val doubleOrderingOps: OrderingOps[V, B]
  val arithmeticRefinementOps: ArithmeticRefinementOps[V]

  override protected def canDetermineValue(t: ir.Term): Boolean = t match
    case IntNum(_) => true
    case DoubleNum(_) => true
    case BinOp(lhs, rhs, _) => canDetermineValue(lhs) && canDetermineValue(rhs)
    case _ => super.canDetermineValue(t)

  private def binaryArithmeticComparison(op: BinaryArithmeticComparisonOperator, ops: OrderingOps[V, B]): (V, V) => B =
    op match
      case BinaryArithmeticComparisonOperator.Leq => ops.le
      case BinaryArithmeticComparisonOperator.Lt => ops.lt
      case BinaryArithmeticComparisonOperator.Gt => ops.gt
      case BinaryArithmeticComparisonOperator.Geq => ops.ge

  private def binaryArithmeticIntOp(op: String): (V, V) => V = op match
    case "+" => intOps.add
    case "-" => intOps.sub
    case "*" => intOps.mul
    case "/" => intOps.div
    case "%" => intOps.remainder
    case "min" => intOps.min
    case "max" => intOps.max

  private def binaryArithmeticDoubleOp(op: String): (V, V) => V = op match
    case "+" => doubleOps.add
    case "-" => doubleOps.sub
    case "*" => doubleOps.mul
    case "/" => doubleOps.div
    case "min" => doubleOps.min
    case "max" => doubleOps.max

  override def evalAtomOpen(at: Atom)(using Fixed): Unit = at match
    case BinCompare(lhs, rhs, opStr)  =>
      val orderingOps = lhs.typ match
        case Some(tt) if tt.ty == TInt => intOrderingOps
        case Some(tt) if tt.ty == TDouble => doubleOrderingOps
        case tt => throw IllegalStateException(s"Unexpected TermType $tt")

      val ls = evalTerm(lhs)
      val rs = evalTerm(rhs)

      val op = BinaryArithmeticComparisonOperator.fromString(opStr)
      val opFun = binaryArithmeticComparison(op, orderingOps)
      updateSupplementaryChecked { sup =>
        val lix = relationOps.columnIndex(sup, ls)
        val rix = relationOps.columnIndex(sup, rs)
        relationOps.filter(sup) { row =>
          opFun(row(lix), row(rix))
        } { row =>
          val (lv, rv) = arithmeticRefinementOps.refine(row(lix), row(rix), op)
          row.updated(lix, lv).updated(rix, rv)
        }
      }
    case _ => super.evalAtomOpen(at)


  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case IntNum(i: Int) => termResult(intOps.integerLit(i))
    case DoubleNum(d: Double) => termResult(doubleOps.floatingLit(d))
    case BinOp(lhs, rhs, op) if term.typ.exists(_.ty == TInt) =>
      binaryOp(evalTerm(lhs), evalTerm(rhs))(binaryArithmeticIntOp(op))
    case BinOp(lhs, rhs, op) if term.typ.exists(_.ty == TDouble) =>
      binaryOp(evalTerm(lhs), evalTerm(rhs))(binaryArithmeticDoubleOp(op))
    case _ => super.evalTermOpen(term)
