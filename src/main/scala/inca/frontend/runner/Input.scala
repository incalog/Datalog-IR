package inca.frontend.runner

import truechange.EditScript

case class EDBChange(es: EditScript, insertions: Seq[Relation], deletions: Seq[Relation])
object EDBChange {
  def empty: EDBChange = EDBChange(EditScript(Seq()), Seq(), Seq())
  def insertions(inserts: Seq[Relation]): EDBChange = EDBChange(EditScript(Seq()), inserts, Seq())
  def deletions(deletes: Seq[Relation]): EDBChange = EDBChange(EditScript(Seq()), Seq(), deletes)
  def structural(es: EditScript): EDBChange = EDBChange(es, Seq(), Seq())
}

trait Input {
  def change: EDBChange
  def args: Relation
}

/*case class StdDatalogInput(override val args: Relation, insertions: RelationChange, deletions: RelationChange) extends Input {
  override def change: EDBChange = EDBChange(EditScript(Seq()), insertions, deletions)
}*/

case class IRInput(override val args: Relation, override val change: EDBChange) extends Input
object IRInput {
  def apply(parameterNames: Seq[String], values: Seq[Any]*): IRInput = {
    values.foreach { v =>
      if (v.size != parameterNames.size)
        throw new IllegalArgumentException(s"Expected ${parameterNames.size} values, but got ${v.size}.")
    }
    // if we use the relation as an input argument for run, we do not need a name, since the runner knows the name
    new IRInput(Relation.from("", parameterNames, values), EDBChange.empty)
  }

  def apply(args: Relation): IRInput = {
    new IRInput(args, EDBChange.empty)
  }

  def apply(change: EDBChange): IRInput = {
    new IRInput(UnitRelation(""), change)
  }
}

case class FunctionalInput(arguments: Seq[meta.Term]) extends Input {
  // TODO we need to compile abstract syntax trees in scala format, how do we get this?
  def args: Relation = ???
  def change: EDBChange = ???
}