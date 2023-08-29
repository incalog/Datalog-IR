package inca.util

import scala.annotation.tailrec
import scala.collection.immutable.MultiDict

object TupleOps {
  /**
   * From: https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Scala
   */
  def cartesianProduct[T](lst: Seq[Seq[T]]): Seq[Seq[T]] = {
    /**
     * Prepend single element to all lists of list
     *
     * @param e  single elemetn
     * @param ll list of list
     * @param a  accumulator for tail recursive implementation
     * @return list of lists with prepended element e
     */
    @tailrec
    def pel(e: T,
            ll: Seq[Seq[T]],
            a: Seq[Seq[T]] = Nil): Seq[Seq[T]] =
      ll.toList match {
        case Nil => a.reverse
        case x :: xs => pel(e, xs, (e +: x) +: a)
      }

    val res = lst.toList match {
      case Nil => Nil
      case x :: Nil => Seq(x)
      case x :: _ =>
        x match {
          case Nil => Nil
          case _ =>
            lst.foldRight(Seq(x))((l, a) =>
              l.flatMap(pel(_, a))
            ).map(_.dropRight(x.size))
        }
    }
    res
  }

  @scala.annotation.tailrec
  def transClosure[T](rel: MultiDict[T, T]): MultiDict[T, T] = {
    val newRel = rel.mapSets { case (src, trg) =>
      src -> (trg ++ trg.flatMap { s => rel.get(s) })
    }
    if (newRel == rel) rel
    else transClosure(newRel)
  }
}
