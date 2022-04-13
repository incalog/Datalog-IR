package inca.frontend.souffle.inputreader

import inca.runtime.db.DatabaseInput
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import truechange.EditScript
import inca.frontend.souffle.compiler.Compiler

class InputToNamedRelationsReader(val dir: String) extends SouffleInputReader {
  override def compileRow(relation: String, row: Seq[(String, Any)]): DatabaseInput = {
    val tuple = Tuples.flatTupleOf(row.map(_._2): _*)
    DatabaseInput(
      EditScript(Seq()),
      Map(Compiler.EXT_PREFIX + relation -> Set(tuple)),
      Map())
  }
}
