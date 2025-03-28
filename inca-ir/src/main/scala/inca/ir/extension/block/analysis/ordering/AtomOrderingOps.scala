package inca.ir.extension.block.analysis.ordering

import inca.ir.{Atom, Eq}
import inca.ir.analysis.base.ordering.{BaseAtomOrderingOps, Priority}
import inca.ir.extension.block.Block

trait AtomOrderingOps extends BaseAtomOrderingOps:
  override def priority(at: Atom): Int = at match
    case Eq(lhs: Block, rhs, neg) => Priority.Normal
    case Eq(lhs, rhs: Block, neg) => Priority.Normal
    case _ => super.priority(at)
