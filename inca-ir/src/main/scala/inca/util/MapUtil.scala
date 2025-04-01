package inca.util

import scala.annotation.tailrec

object MapUtil:
  @tailrec
  def transClosure[T](rel: Map[T, Set[T]]): Map[T, Set[T]] = {
    val newRel = rel.map { case (src, trg) =>
      src -> (trg ++ trg.flatMap { s => rel.getOrElse(s, Seq()) })
    }
    if (newRel == rel) rel
    else transClosure(newRel)
  }
