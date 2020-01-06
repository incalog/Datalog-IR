package org.inca.diff.changeset

import org.inca.diff.Diffable

import scala.collection.mutable

object SimpleChangesetApi extends ChangesetApi[Diffable[_], String, Class[_]]

class ChangesetApi[TNode,TNodeRef,TNodeTag] {
  type Changeset = Seq[ChangeCmd]
  class ChangesetBuffer(val buf: mutable.Buffer[ChangeCmd], val gensym: Gensym = new Gensym) {
    def += (elem: ChangeCmd): this.type = {buf += elem; this}
    def ++= (elem: IterableOnce[ChangeCmd]): this.type = {buf ++= elem; this}
    def freshVar(): Var = gensym.fresh()
  }
  type Node = TNode
  type NodeTag = TNodeTag

  trait NodeRef
  case class URI(id: TNodeRef) extends NodeRef
  case class Literal[T](value: T) extends NodeRef
  case class Var(name: String) extends NodeRef {
    override def toString: String = name
  }

  trait ChangeCmd
  case class LoadNode(v: Var, node: NodeTag, kids: Iterable[(Link, NodeRef)]) extends ChangeCmd {
    override def toString: String = s"$v = LoadNode($node, $kids)"
  }
  case class UnloadNode(ref: NodeRef) extends ChangeCmd
  case class AttachNode(parent: NodeRef, l: Link, newchild: NodeRef) extends ChangeCmd
  case class DetachNode(ref: NodeRef) extends ChangeCmd

  sealed trait Link
  case object RootLink extends Link
  case class NamedLink(name: String) extends Link
  case class ListIndexLink(list: Link, at: Int) extends Link

  class Gensym {
    private var count: Int = 0
    def fresh(): Var = {
      val name = s"x_$count"
      count += 1
      Var(name)
    }
  }
}
