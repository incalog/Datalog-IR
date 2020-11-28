package inca.frontend.extensions.evalCall

import inca.frontend.core
import inca.frontend.desugar.Desugarable

trait Frontend extends core.Frontend with Parser with Typechecker {
  override protected def desugarables: Seq[Desugarable] = Desugaring +: super.desugarables
}
