package inca.util

import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, Tuples}

object TupleOps {
  def binaryFlip(tuple: ITuple): ITuple =
    Tuples.staticArityFlatTupleOf(tuple.get(1), tuple.get(0))

  def binaryTuple(tuple: ITuple): Tuple = tuple match {
    case t: Tuple => t
    case _ => Tuples.staticArityFlatTupleOf(tuple.get(0), tuple.get(1))
  }

}
