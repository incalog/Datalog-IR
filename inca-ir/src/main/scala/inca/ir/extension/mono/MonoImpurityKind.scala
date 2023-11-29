package inca.ir.extension.mono

import inca.ir.Type
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.impure.ImpurityKind

object MonoImpurityKind extends ImpurityKind:
  override val name: String = "MonoImpurity"
  override val ty: Type = TInt

