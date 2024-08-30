package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir.{Atom, Term}
import inca.ir.analysis.base.interpreter.BaseAbstractInterpreter
import inca.ir.analysis.base.values.{VBool, Value}
import inca.ir.extension.arithmetic.analysis.ordering.{DoubleVOrderingOps, IntVOrderingOps}
import inca.ir.extension.arithmetic.{BinCompare, BinOp, DoubleNum, IntNum, TDouble, TInt, UnOp}
import inca.ir.extension.arithmetic.analysis.values.{DoubleVOps, IntVOps}
import sturdy.effect.failure.Failure
import sturdy.values.integer.IntegerOps
import sturdy.values.floating.FloatOps
import sturdy.values.ordering.OrderingOps

// Constant Analysis
trait ConstantAbstractInterpreter extends AbstractInterpreter:
  val intOps: IntegerOps[Int, Value] = IntVOps(using failure, effects)
  val doubleOps: FloatOps[Double, Value] = DoubleVOps(using failure, effects)
  val intOrderingOps: OrderingOps[Value, VBool] = IntVOrderingOps()
  val doubleOrderingOps: OrderingOps[Value, VBool] = DoubleVOrderingOps()

trait AbstractInterpreter extends BaseAbstractInterpreter:
  val intOps: IntegerOps[Int, Value]
  val doubleOps: FloatOps[Double, Value]
  val intOrderingOps: OrderingOps[Value, VBool]
  val doubleOrderingOps: OrderingOps[Value, VBool]

  override def evalAtom(at: Atom)(using Fixed): Unit = at match
    case BinCompare(t1, t2, op) =>
      val tr1 = evalTerm(t1)
      val tr2 = evalTerm(t2)
      val resRV = relationOps.natJoin(tr1.asTable("_$TermResult1"), tr2.asTable("_$TermResult2"))
      val ix1 = relationOps.getCols(resRV).indexOf("_$TermResult1")
      val ix2 = relationOps.getCols(resRV).indexOf("_$TermResult2")
      val compRes = (op, t1.typ.get.ty) match
        case ("<", TInt) => relationOps.filter(resRV, vec => intOrderingOps.lt(vec(ix1), vec(ix2)))
        case ("<", TDouble) => relationOps.filter(resRV, vec => doubleOrderingOps.lt(vec(ix1), vec(ix2)))
        case ("<=", TInt) => relationOps.filter(resRV, vec => intOrderingOps.le(vec(ix1), vec(ix2)))
        case ("<=", TDouble) => relationOps.filter(resRV, vec => doubleOrderingOps.le(vec(ix1), vec(ix2)))
        case (">", TInt) => relationOps.filter(resRV, vec => intOrderingOps.gt(vec(ix1), vec(ix2)))
        case (">", TDouble) => relationOps.filter(resRV, vec => doubleOrderingOps.gt(vec(ix1), vec(ix2)))
        case (">=", TInt) => relationOps.filter(resRV, vec => intOrderingOps.ge(vec(ix1), vec(ix2)))
        case (">=", TDouble) => relationOps.filter(resRV, vec => doubleOrderingOps.ge(vec(ix1), vec(ix2)))
      merge(relationOps.projection(compRes, relationOps.getCols(compRes).filter(s => s != "_$TermResult1" && s != "_$TermResult2")), false)
    case _ => super.evalAtom(at)

  override def evalTermExtend(term: Term)(using Fixed): TermResult =
    val currSup = supplementaryTable.getTable
    term match
      case IntNum(i) => TermResult(currSup, relationOps.scan(currSup)(_ => intOps.integerLit(i)), VBool.True)
      case DoubleNum(d) => TermResult(currSup, relationOps.scan(currSup)(_ => doubleOps.floatingLit(d)), VBool.True)
      case BinOp(t1, t2, op) =>
        val tr1 = evalTerm(t1)
        val tr2 = evalTerm(t2)
        (op, term.typ.get.ty) match
          case ("+", TInt) => tr1.combineWith(tr2, intOps.add)
          case ("+", TDouble) => tr1.combineWith(tr2, doubleOps.add)
          case ("-", TInt) => tr1.combineWith(tr2, intOps.sub)
          case ("-", TDouble) => tr1.combineWith(tr2, doubleOps.sub)
          case ("*", TInt) => tr1.combineWith(tr2, intOps.mul)
          case ("*", TDouble) => tr1.combineWith(tr2, doubleOps.mul)
          case ("/", TInt) => tr1.combineWith(tr2, intOps.div)
          case ("/", TDouble) => tr1.combineWith(tr2, doubleOps.div)
          case ("%", TInt) => tr1.combineWith(tr2, intOps.remainder)
          case ("min", TInt) => tr1.combineWith(tr2, intOps.min)
          case ("min", TDouble) => tr1.combineWith(tr2, doubleOps.min)
          case ("max", TInt) => tr1.combineWith(tr2, intOps.max)
          case ("max", TDouble) => tr1.combineWith(tr2, doubleOps.max)
      case UnOp(t, op) =>
        val tr = evalTerm(t)
        (op, term.typ.get.ty) match
          case ("abs", TInt) => tr.mapResult(intOps.absolute)
          case ("abs", TDouble) => tr.mapResult(doubleOps.absolute)
      case _ => super.evalTermExtend(term)
