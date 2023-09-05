package inca.frontend.souffle.lowering

import inca.runtime.db.DatabaseInput
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import truechange.EditScript

class SouffleToNamedRelations(val dir: String) extends SouffleInputReader {
  override def compileRow(relation: String, row: Seq[(String, Any)]): DatabaseInput = {
    val tuple = Tuples.flatTupleOf(row.map(_._2): _*)
    DatabaseInput(
      EditScript(Seq()),
      Map(SouffleToDatalogIR.ExtensionalCallPrefix + relation -> Set(tuple)),
      Map())

  }
}
