package inca.ir.extension.set.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.set.analysis.interpreter.{ConstantSetV}
import inca.ir.extension.set as irset
import inca.ir.*
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantSetV(values) => Some(irset.SetLit(values.toSeq.flatMap(valueToTerm.apply)))
    case _ => super.valueToTermInternal(value)

  /*override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case irset.SetMember(mem, s) if mem.typ.exists(_.mode.isBound) => ???
      case irset.SetMember(mem, s) if mem.typ.exists(_.mode.isBinding) => ???
      case _ => super.visitAtom(atom)
  }*/



