package org.inca.diff

trait DiffableOracle {
  def predict[T <: Diffable[_]](t: T): Option[MetaVar[T]]
}
