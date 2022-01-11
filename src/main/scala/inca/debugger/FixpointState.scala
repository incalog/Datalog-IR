package inca.debugger

import inca.backend.ir.Datalog
import inca.debugger.table.Table

case class FixpointState(derived: Map[Datalog.Name, Table]) {
  def addRelation(name: Datalog.Name, relation: Table): FixpointState = {
    FixpointState(derived + (name -> relation))
  }

  def extendRelation(name: Datalog.Name, table: Table)(implicit patterns: Map[Datalog.Name, Datalog.Pattern]): FixpointState = {
    val rel = derived.getOrElse(name, Table(patterns(name).params.map(_.name).toVector, Vector()))
    val newRels = derived + (name -> rel.addRows(table))
    FixpointState(newRels)
  }
}
