package inca.debugger

import inca.backend.ir.Datalog

case class FixpointState(derivedRels: Map[Datalog.Name, Table]) {
  def addRelation(name: Datalog.Name, relation: Table): FixpointState = {
    FixpointState(derivedRels + (name -> relation))
  }

  def extendRelation(name: Datalog.Name, relation: Table)(implicit patterns: Map[Datalog.Name, Datalog.Pattern]): FixpointState = {
    val rel = derivedRels.getOrElse(name, Table(patterns(name).params.map(_.name).toVector, Vector()))
    val newRels = derivedRels + (name -> rel.addRows(relation.data))
    FixpointState(newRels)
  }
}
