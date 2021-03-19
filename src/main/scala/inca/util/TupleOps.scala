package inca.util

import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, Tuples}

import scala.collection.immutable.MultiDict

object TupleOps {
  def binaryFlip(tuple: ITuple): Tuple =
    Tuples.staticArityFlatTupleOf(tuple.get(1), tuple.get(0))

  def binaryTuple(tuple: ITuple): Tuple = tuple match {
    case t: Tuple => t
    case _ => Tuples.staticArityFlatTupleOf(tuple.get(0), tuple.get(1))
  }

  @scala.annotation.tailrec
  def transClosure[T](rel: MultiDict[T, T]): MultiDict[T, T] = {
    val newRel = rel.mapSets { case (src, trg) =>
      src -> (trg ++ trg.flatMap { s => rel.get(s) } )
    }
    if (newRel == rel) rel
    else transClosure(newRel)
  }
}
