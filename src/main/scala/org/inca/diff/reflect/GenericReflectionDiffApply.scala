package org.inca.diff.reflect

import GenericReflectionDiff._

object GenericReflectionDiffApply {

  def applyPatch(p: Patch, node: Any): Option[Any] = p match {
    case Hole(change) => applyChange(change, node)
    case ValC(v) if v == node => Some(node)
    case NodeC(cls, subs) if cls == node.getClass =>
      val newnode = cls.newInstance()
      (subs zip cls.allFields).foreach { tt =>
        val fld = tt._2
        val subnode = fld.get(node)
        applyPatch(tt._1, subnode) match {
          case Some(newSubnode) => fld.set(newnode, subnode)
          case None => return None
        }
      }
      Some(newnode)
  }

  def applyChange(c: Change[MetaVar], t: Any): Option[Any] =
    del(c.delCtx, t) flatMap (ins(c.insCtx, _))

  def del(ctx: TreeC[MetaVar], tree: Any): Option[Map[MetaVar, Any]] =
    go(ctx, tree, Map())

  def go(ctx: TreeC[MetaVar], node: Any, m: Map[MetaVar, Any]): Option[Map[MetaVar, Any]] = ctx match {
    case ValC(v) => if (v == node) Some(m) else None
    case NodeC(cls, subs) =>
      if (cls != node.getClass)
        None
      else {
        var res = m
        (subs zip cls.allFieldVals(node)).foreach { tt =>
          go(tt._1, tt._2, res) match {
            case Some(m_) => res = m_
            case None => return None
          }
        }
        Some(res)
      }
    case Hole(i) => m.get(i) match {
      case None => Some(m + (i -> node))
      case Some(node_) => if(node == node_) Some(m) else None
    }
  }


  def ins(ctx: TreeC[MetaVar], m: Map[MetaVar, Any]): Option[Any] = ctx match {
    case ValC(v) => Some(v)
    case NodeC(cls, subs) =>
      val node = cls.newInstance()
      (subs zip cls.allFields).foreach { tt =>
        ins(tt._1, m) match {
          case Some(subnode) => tt._2.set(node, subnode)
          case None => return None
        }
      }
      Some(node)
    case Hole(i) => m.get(i)
  }
}
