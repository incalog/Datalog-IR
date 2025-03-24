package inca.ir.extension.map.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.map.analysis.interpreter.{ConstantMapV, ConstantMapFunV}
import inca.ir.extension.map as irmap
import inca.ir.*
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def mayEliminate(t: Term): Boolean = t match
    case _: irmap.MapComprehension => false // contains atoms that might fail
    case _ => super.mayEliminate(t)

  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantMapV(values) => Some(irmap.MapLit(values.toSeq.flatMap {
      (k, vs) => valueToTerm(k) match
        case Some(key) =>
          vs.toSeq.flatMap { v =>
            valueToTerm(v).map(key -> _)
          }
        case None => Seq()
      }))
    case ConstantMapFunV(_) => None
    case _ => super.valueToTermInternal(value)

  /*override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case irmap.MapContains(m, k) if k.typ.exists(_.mode.isBound) => ???
      case irmap.MapContains(m, k) if k.typ.exists(_.mode.isBinding) => ???
      case _ => super.visitAtom(atom)
  }*/



