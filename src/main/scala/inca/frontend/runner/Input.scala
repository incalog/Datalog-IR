package inca.frontend.runner

import inca.frontend.runner.EDBChange.RelationChange
import truechange.EditScript

case class EDBChange(es: EditScript, insertions: RelationChange, deletions: RelationChange)
object EDBChange {
  type RelationChange = Map[RelationName, Relation]
  def empty: EDBChange = EDBChange(EditScript(Seq()), Map(), Map())
  def insertions(inserts: RelationChange): EDBChange = EDBChange(EditScript(Seq()), inserts, Map())
  def deletions(deletes: RelationChange): EDBChange = EDBChange(EditScript(Seq()), Map(), deletes)
  def structural(es: EditScript): EDBChange = EDBChange(es, Map(), Map())
}

trait Input {
  def change: EDBChange
  def args: Relation
}

case class StdDatalogInput(arguments: Relation, insertions: RelationChange, deletions: RelationChange) extends Input {
  override def change: EDBChange = EDBChange(EditScript(Seq()), insertions, deletions)
  override def args: Relation = arguments
}
case class IRInput(override val args: Relation, override val change: EDBChange) extends Input

case class FunctionalInput(arguments: Seq[meta.Term]) extends Input {
  // TODO we need to compiled abstract syntax trees in scala format, how do we get this?
  def args: Relation = ???
  def change: EDBChange = ???
}