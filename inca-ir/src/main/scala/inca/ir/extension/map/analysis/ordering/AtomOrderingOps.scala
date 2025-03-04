package inca.ir.extension.map.analysis.ordering

import inca.ir.Atom
import inca.ir.analysis.base.ordering.{AtomOrderingOps, BaseAtomOrderingOps, Priority, given}
import inca.ir.extension.data.Deconstruct

trait AtomOrderingOps extends BaseAtomOrderingOps:
  override def priority(at: Atom): Int = at match
    case _: Deconstruct => Priority.High
    case _ => super.priority(at)
