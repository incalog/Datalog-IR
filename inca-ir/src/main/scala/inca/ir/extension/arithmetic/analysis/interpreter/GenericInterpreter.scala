package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir
import inca.ir.Atom
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.arithmetic.*
import sturdy.data.MayJoin
import sturdy.values.floating.FloatOps
import sturdy.values.integer.IntegerOps
import sturdy.values.ordering.OrderingOps

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val intOps: IntegerOps[Int, V]
  val doubleOps: FloatOps[Double, V]
  val intOrderingOps: OrderingOps[V, B]
  val doubleOrderingOps: OrderingOps[V, B]

  override protected def canDetermineValue(t: ir.Term): Boolean = t match
    case IntNum(_) => true
    case DoubleNum(_) => true
    case BinOp(lhs, rhs, _) => canDetermineValue(lhs) && canDetermineValue(rhs)
    case _ => super.canDetermineValue(t)

  private def binaryArithmeticComparison(op: String, ops: OrderingOps[V, B]): (V, V) => B = op match
    case "<=" => ops.le
    case "<" => ops.lt
    case ">" => ops.gt
    case ">=" => ops.ge

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
    case BinCompare(lhs, rhs, op)  =>
      val orderingOps = lhs.typ match
        case Some(tt) if tt.ty == TInt => intOrderingOps
        case Some(tt) if tt.ty == TDouble => doubleOrderingOps
        case tt => throw IllegalStateException(s"Unexpected TermType $tt")

      val ls = evalTerm(lhs)
      val rs = evalTerm(rhs)

      val opFun = binaryArithmeticComparison(op, orderingOps)
      updateSupplementaryChecked { sup =>
        val lix = relationOps.columnIndex(sup, ls)
        val rix = relationOps.columnIndex(sup, rs)
        relationOps.filter(sup) { row => opFun(row(lix), row(rix)) }
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
