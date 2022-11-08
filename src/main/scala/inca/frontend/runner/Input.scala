package inca.frontend.runner

import truechange.EditScript

trait EDBChange {
  def es: EditScript
  def insertions: Map[RelationName, Relation]
  def deletions: Map[RelationName, Relation]
}

trait Input {
  def translate: EDBChange
}

case class IRInput(rel: Relation) extends Input {
  def translate: EDBChange = ???
}