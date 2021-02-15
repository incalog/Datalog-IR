package inca.frontend_old.extensions.forallExists

import inca.frontend_old.core
import inca.frontend_old.desugar.Desugarable

trait Frontend extends core.Frontend with Parser with Typechecker {
  override protected def desugarables: Seq[Desugarable] = Desugaring +: super.desugarables
}
