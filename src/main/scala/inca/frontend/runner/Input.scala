package inca.frontend.runner

import inca.compiler.CompiledModule
import inca.frontend.Constants.RelationName
import truechange.EditScript

import scala.meta.Self

case class EDBChange(es: EditScript, insertions: Seq[Relation], deletions: Seq[Relation])
object EDBChange {
  def empty: EDBChange = EDBChange(EditScript(Seq()), Seq(), Seq())
  def insertions(inserts: Seq[Relation]): EDBChange = EDBChange(EditScript(Seq()), inserts, Seq())
  def deletions(deletes: Seq[Relation]): EDBChange = EDBChange(EditScript(Seq()), Seq(), deletes)
  def structural(es: EditScript): EDBChange = EDBChange(es, Seq(), Seq())
}

protected[frontend] trait Input {
  def change: EDBChange
  def args: Relation
}

protected[frontend] trait InputObject[I <: Input] {
  type InputClosure = (CompiledModule, RelationName) => I
}

/*case class StdDatalogInput(override val args: Relation, insertions: RelationChange, deletions: RelationChange) extends Input {
  override def change: EDBChange = EDBChange(EditScript(Seq()), insertions, deletions)
}*/

final case class IRInput private (override val args: Relation, override val change: EDBChange) extends Input
object IRInput extends InputObject[IRInput] {
  def apply(parameterNames: Seq[String], values: Seq[Any]*): InputClosure = (_, relName) => {
      values.foreach { v =>
        if (v.size != parameterNames.size)
          throw new IllegalArgumentException(s"Expected ${parameterNames.size} values, but got ${v.size}.")
      }
      // if we use the relation as an input argument for run, we do not need a name, since the runner knows the name
      IRInput(Relation.from(relName, parameterNames, values), EDBChange.empty)
    }

  def args(args: Relation): InputClosure = (_, _) => {
    IRInput(args, EDBChange.empty)
  }

  def empty(): InputClosure = (_, relName) => {
    IRInput(UnitRelation(relName), EDBChange.empty)
  }
}

/*case class FunctionalInput(arguments: Seq[meta.Term]) extends Input {
  // TODO we need to compile abstract syntax trees in scala format, how do we get this?
  def args: Relation = ???
  def change: EDBChange = ???
}*/