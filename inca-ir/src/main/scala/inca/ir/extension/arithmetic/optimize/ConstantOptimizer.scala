package inca.ir.extension.arithmetic.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.arithmetic.analysis.interpreter.{ConstantDoubleV, ConstantIntV}
import inca.ir.extension.arithmetic as irarith
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.optimize.ConstantBaseIROptimizer
import sturdy.values.Topped
import sturdy.values.ordering.OrderingOps
import inca.ir.optimize.isTrue

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  val intOrderingOps: OrderingOps[Value, Topped[Boolean]] = abstractInterpreter.intOrderingOps

  override def mayEliminate(t: Term): Boolean = t match
    case irarith.IntNum(_) | irarith.DoubleNum(_) => isConstant(t)
    case irarith.BinOp(lhs, rhs, _) => isConstant(t) && mayEliminate(lhs) && mayEliminate(rhs)
    case _ => super.mayEliminate(t)

  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantIntV(v1) => Some(irarith.IntNum(v1))
    case ConstantDoubleV(v1) => Some(irarith.DoubleNum(v1))
    case _ => super.valueToTermInternal(value)

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      // Remove comparisons constraints that always hold
      case irarith.BinCompare(lhs, rhs, op) =>
        val opFun = op match
          case "<" => intOrderingOps.lt
          case "<=" => intOrderingOps.le
          case ">" => intOrderingOps.gt
          case ">=" => intOrderingOps.ge
        if (binCompare(lhs, rhs, opFun(_, _)).exists(_.isTrue))
          Seq()
        else
          super.visitAtom(atom)
      case _ => super.visitAtom(atom)
  }



