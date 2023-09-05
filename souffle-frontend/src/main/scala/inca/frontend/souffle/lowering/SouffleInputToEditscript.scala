package inca.frontend.souffle.lowering

import inca.runtime.db.DatabaseInput
import truechange.EditScript
import truechange.JVMURI
import truechange.Load
import truechange.NamedTag

// Important: Legacy souffle code .type Type will translate to .type Type <: symbol
class SouffleInputToEditscript(val dir: String) extends SouffleInputReader {
  def compileRow(relation: String, row: Seq[(String, Any)]): DatabaseInput = {
    val tag = NamedTag(relation.intern)
    DatabaseInput(EditScript(Seq(Load(new JVMURI, tag, Seq(), row))), Map(), Map())
  }
}
