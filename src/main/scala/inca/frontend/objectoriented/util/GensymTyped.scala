package inca.frontend.objectoriented.util

import inca.frontend.objectoriented.core.Type
import inca.util.Gensym

/**
 * Gensym subclass that allows storing optional type information for each variable.
 */
class GensymTyped(init: Iterable[(String, Option[Type])]) extends Gensym(Iterable.empty) {
  /** map of used symbols, each of which must end with '$' */
  private[inca] var types: Map[String, Type] = Map()

  init.foreach { case(name, typOption) => registerWithType(name, typOption) }

  def registerWithTypes(it: Iterable[(String, Option[Type])]): Unit =
    it.foreach { case (name, typ) => registerWithType(name, typ) }

  def registerWithType(s: String, t: Option[Type]): Unit = {
    decompileName(s) match {
      case (s_, None) =>
        used += s_ -> used.getOrElse(s_, 0)
        if (t.isDefined)
          types += s_ -> t.get
      case (s_, Some(num)) =>
        used += s_ -> (num + 1).max(used.getOrElse(s_, 0))
        if (t.isDefined)
          types += s_ -> t.get
    }
  }

  def get(base: String): Option[(String, Option[Type])] = {
    val base_ = decompileName(base)._1
    used.get(base_) match {
      case Some(count) =>
        val num = (count - 1)
        val typ = types.get(base_)
        if (num < 0)
          Some((base_.dropRight(1), typ))
        else
          Some((base_ + num), typ)
      case None =>
        None
    }
  }

  def snapshot: GensymTyped = {
    val snapshot = new GensymTyped(Iterable.empty)
    snapshot.used = this.used
    snapshot.globals = this.globals
    snapshot.types = this.types
    snapshot
  }

  def diff(that: GensymTyped): GensymTyped = {
    val diff = new GensymTyped(Iterable.empty)
    diff.used = this.used.flatMap { case (k, v) =>
      val newV = that.used.get(k)
      if (newV.isDefined) {
        if (newV.get == v) {
          None
        } else {
          Seq(k -> v)
        }
      } else {
        Seq(k -> v)
      }
    }
    val keys = diff.used.keySet
    diff.types = this.types.filter { case(k, _) => keys.contains(k) }
    diff.globals = this.globals.toSet.diff(that.globals.toSet).toSeq
    diff
  }

  def union(that: GensymTyped): GensymTyped = {
    val union = new GensymTyped(Iterable.empty)
    val keySet = this.used.keySet.union(that.used.keySet)
    val valuesWithType = keySet.map { k =>
      val v1 = this.used.get(k)
      val v2 = that.used.get(k)
      if (v1.isDefined && v2.isDefined) {
        k -> (v1.get.max(v2.get), this.types.get(k))
      } else if (v1.isDefined) {
        k -> (v1.get, this.types.get(k))
      } else {
        k -> (v2.get, that.types.get(k))
      }
    }.toMap
    union.types = valuesWithType.flatMap { case (k, (_ , typ)) => if (typ.isDefined) Seq(k -> typ.get) else None }
    union.used = valuesWithType.map { case (k, (n , _)) => k -> n }
    union.globals = this.globals.toSet.union(that.globals.toSet).toSeq
    union
  }

  def symbols: Set[String] = {
    this.used.keySet.map(_.dropRight(1)) // drop the $ symbol
  }
}
