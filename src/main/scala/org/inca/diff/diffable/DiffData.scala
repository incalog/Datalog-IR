package org.inca.diff.diffable

import org.inca.diff.diffable.DiffData.{DiffableContext, DiffableNoHoles, Patch}
import org.inca.diff.diffable.Diffable.DiffableData

object DiffData {
  type DiffableNoHoles = DiffableData[Nothing]
  type DiffableContext = DiffableData[MetaVar]
  type VarMap = Map[MetaVar, DiffableNoHoles]
  type Patch = DiffableData[Change]
}

trait Plug {
  val freevars: Set[MetaVar]
}

class MetaVar(val i: Int) extends Plug {
  override val freevars: Set[MetaVar] = Set(this)

  override def toString: String = i.toString
  override def equals(obj: Any): Boolean = obj.isInstanceOf[MetaVar] && i==obj.asInstanceOf[MetaVar].i
  override def hashCode(): Int = i
}

case class Change(delCtx: DiffableContext, insCtx: DiffableContext) extends Plug {
  override lazy val freevars: Set[MetaVar] = insCtx.freevars diff delCtx.freevars
  def isClosed: Boolean = freevars.isEmpty
}