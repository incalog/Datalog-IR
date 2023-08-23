package inca.ir.extension.set

import inca.ir.Hint
import inca.ir.Hint.Key

object Hints {
  val RefunctionalizeKey = "REFUNCTIONALIZE"

  // TODO: We could make this way nicer (and more complicated) by implementing a dataflow analysis.
  /**
   * Force a refunctionalization.
   * This is useful, if we for example want to refunctionalize a return parameter.
   * In a frontend it is obvious what is input and what is output. This information is not obvious in the IR.
   *  Note: You can only add this hint to params and atoms
   * @param vars the name of all params / vars you manually refunctionalized
   */
  case class Refunctionalize(vars: Seq[String] = Seq()) extends Hint {
    val key: Key = RefunctionalizeKey
  }
}
