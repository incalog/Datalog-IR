package inca.ir.extension.set

import inca.ir.Hint
import inca.ir.Hint.Key

object Hints {
  val RefunctionalizeKey = "REFUNCTIONALIZE"

  // Force a refunctionalization
  // This is useful, if we for example want to refunctionalize a return parameter.
  // In a frontend it is obvious what is input and what is output. This information is not obvious in the IR.
  case object Refunctionalize extends Hint {
    val key: Key = RefunctionalizeKey
  }
}
