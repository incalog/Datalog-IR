package inca.frontend.ir

import truechange.EditScript

case class EDBChange(es: EditScript, insertions: Seq[Relation], deletions: Seq[Relation])
object EDBChange {
  def empty: EDBChange = EDBChange(EditScript(Seq()), Seq(), Seq())
  def insertions(inserts: Seq[Relation]): EDBChange = EDBChange(EditScript(Seq()), inserts, Seq())
  def deletions(deletes: Seq[Relation]): EDBChange = EDBChange(EditScript(Seq()), Seq(), deletes)
  def structural(es: EditScript): EDBChange = EDBChange(es, Seq(), Seq())
}