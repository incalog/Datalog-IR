package org.inca.diff.tree23

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import org.inca.diff.WithCachedCryptoHash

object Tree23 {

  val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

  trait Tree23 extends WithCachedCryptoHash
  case class Leaf(s: String) extends Tree23 {
    override val hash: Array[Byte] = {
      digest.update(0:Byte)
      digest.update(s.getBytes(StandardCharsets.UTF_8))
      digest.digest()
    }
  }
  case class Node2(t1: Tree23, t2: Tree23) extends Tree23 {
    override val hash: Array[Byte] = {
      digest.update(1:Byte)
      digest.update(t1.hash)
      digest.update(t2.hash)
      digest.digest()
    }
  }
  case class Node3(t1: Tree23, t2: Tree23, t3: Tree23) extends Tree23 {
    override val hash: Array[Byte] = {
      digest.update(2:Byte)
      digest.update(t1.hash)
      digest.update(t2.hash)
      digest.update(t3.hash)
      digest.digest()
    }
  }


  class MetaVar(val i: Int) extends Plug {
    override val freevars: Set[MetaVar] = Set(this)

    override def toString: String = i.toString
    override def equals(obj: Any): Boolean = obj.isInstanceOf[MetaVar] && i==obj.asInstanceOf[MetaVar].i
    override def hashCode(): Int = i
  }

  trait Plug {
    val freevars: Set[MetaVar]
  }

  trait Tree23C[A] {
    val freevars: Set[MetaVar]
    def isClosed: Boolean = freevars.isEmpty
  }
  case class Hole[A <: Plug](a: A) extends Tree23C[A] {
    override val freevars: Set[MetaVar] = a.freevars
  }
  case class LeafC[A <: Plug](s: String) extends Tree23C[A] {
    override val freevars: Set[MetaVar] = Set()
  }
  case class Node2C[A <: Plug](t1: Tree23C[A], t2: Tree23C[A]) extends Tree23C[A] {
    override val freevars: Set[MetaVar] = t1.freevars ++ t2.freevars
  }
  case class Node3C[A <: Plug](t1: Tree23C[A], t2: Tree23C[A], t3: Tree23C[A]) extends Tree23C[A] {
    override val freevars: Set[MetaVar] = t1.freevars ++ t2.freevars ++ t3.freevars
  }

  def asCtx[A <: Plug](t: Tree23): Tree23C[A] = t match {
    case Leaf(s) => LeafC(s)
    case Node2(t1, t2) => Node2C(asCtx(t1), asCtx(t2))
    case Node3(t1, t2, t3) => Node3C(asCtx(t1), asCtx(t2), asCtx(t3))
  }

  def retainHoles[A <: Plug](tc: Tree23C[A], vs: Set[A], t: Tree23): Tree23C[A] = tc match {
    case Hole(a) => if (vs.contains(a)) tc else asCtx(t)
    case LeafC(s) => tc
    case Node2C(t1, t2) => {
      val tnode2 = t.asInstanceOf[Node2]
      Node2C(retainHoles(t1, vs, tnode2.t1), retainHoles(t2, vs, tnode2.t2))
    }
    case Node3C(t1, t2, t3) => {
      val tnode3 = t.asInstanceOf[Node3]
      Node3C(retainHoles(t1, vs, tnode3.t1), retainHoles(t2, vs, tnode3.t2), retainHoles(t3, vs, tnode3.t3))
    }
  }

  case class Change23[A <: Plug](delCtx: Tree23C[A], insCtx: Tree23C[A]) extends Plug {
    override val freevars: Set[MetaVar] = insCtx.freevars diff delCtx.freevars
    def isClosed: Boolean = freevars.isEmpty
  }

  def applyChange(c: Change23[MetaVar], t: Tree23): Option[Tree23] =
    del(c.delCtx, t) flatMap (ins(c.insCtx, _))

  def del(ctx: Tree23C[MetaVar], tree: Tree23): Option[Map[MetaVar, Tree23]] =
    go(ctx, tree, Map())

  def go(ctx: Tree23C[MetaVar], tree: Tree23, m: Map[MetaVar, Tree23]): Option[Map[MetaVar, Tree23]] = (ctx, tree) match {
    case (LeafC(s1), Leaf(s2)) if s1 == s2 => Some(m)
    case (Node2C(x, y), Node2(a, b)) => go(x, a, m) flatMap (go(y, b, _))
    case (Node3C(x, y, z), Node3(a, b, c)) => go(x, a, m) flatMap (go(y, b, _)) flatMap (go(z, c, _))
    case (Hole(i), t) => m.get(i) match {
      case None => Some(m + (i -> t))
      case Some(t_) => if(t == t_) Some(m) else None
    }
    case _ => None
  }


  def ins(ctx: Tree23C[MetaVar], m: Map[MetaVar, Tree23]): Option[Tree23] = ctx match {
    case LeafC(s) => Some(Leaf(s))
    case Node2C(x, y) => for (
        a <- ins(x, m);
        b <- ins(y, m)
      ) yield Node2(a, b)
    case Node3C(x, y, z) => for (
        a <- ins(x, m);
        b <- ins(y, m);
        c <- ins(z, m)
      ) yield Node3(a, b, c)
    case Hole(i) => m.get(i)
  }


  def changeTree23(src: Tree23, dest: Tree23, Oracle23: Tree23Oracle): Change23[MetaVar] = {
    val change = Change23(extract(Oracle23, src), extract(Oracle23, dest))
    postprocess(src, dest, change)
  }

  def extract(Oracle23: Tree23Oracle, t: Tree23): Tree23C[MetaVar] = Oracle23.predict(t) match {
    case Some(i) => Hole(i)
    case None => t match {
      case Leaf(s) => LeafC(s)
      case Node2(a, b) => Node2C(extract(Oracle23, a), extract(Oracle23, b))
      case Node3(a, b, c) => Node3C(extract(Oracle23, a), extract(Oracle23, b), extract(Oracle23, c))
    }
  }

  def postprocess(src: Tree23, dest: Tree23, c: Change23[MetaVar]): Change23[MetaVar] = {
    val okvars = c.delCtx.freevars intersect c.insCtx.freevars
    val postDel = retainHoles(c.delCtx, okvars, src)
    val postIns = retainHoles(c.insCtx, okvars, dest)
    Change23(postDel, postIns)
  }



  type Patch23 = Tree23C[Change23[MetaVar]]
  def isEmptyPatch(p: Patch23): Boolean = p match {
    case Hole(Change23(Hole(i), Hole(j))) => i ==j
    case _ => false
  }

  type Prefix[A <: Plug] = Either[Tree23C[Change23[A]], Change23[A]]

  def mkPrefix[A <: Plug](change: Change23[A]): Prefix[A] =
    if (change.isClosed)
      Left(Hole(change))
    else
      Right(change)

  def greatestCommonClosedPrefix[A <: Plug](t1: Tree23C[A], t2: Tree23C[A]): Prefix[A] = (t1, t2) match {
    case (LeafC(s1), LeafC(s2)) if s1 == s2 => Left(LeafC(s1))
    case (Node2C(a1, b1), Node2C(a2, b2)) =>
      val patch1 = greatestCommonClosedPrefix(a1, a2)
      val patch2 = greatestCommonClosedPrefix(b1, b2)
      (patch1, patch2) match {
        case (Left(tc1), Left(tc2)) => Left(Node2C(tc1, tc2))
        case _ => mkPrefix(Change23(t1, t2))
      }
    case (Node3C(a1, b1, c1), Node3C(a2, b2, c2)) =>
      val patch1 = greatestCommonClosedPrefix(a1, a2)
      val patch2 = greatestCommonClosedPrefix(b1, b2)
      val patch3 = greatestCommonClosedPrefix(c1, c2)
      (patch1, patch2, patch3) match {
        case (Left(tc1), Left(tc2), Left(tc3)) => Left(Node3C(tc1, tc2, tc3))
        case _ => mkPrefix(Change23(t1, t2))
      }
    case _ => mkPrefix(Change23(t1, t2))
  }




  def diffTree23(t1: Tree23, t2: Tree23)(implicit mkOracle23: MkTree32Oracle): Patch23 = {
    val Oracle23 = mkOracle23(t1, t2)
    val change = changeTree23(t1, t2, Oracle23)
    greatestCommonClosedPrefix(change.delCtx, change.insCtx).left.getOrElse(sys.error(s"Unclosable change $change"))
  }

  def applyPatch23(p: Patch23, t: Tree23): Option[Tree23] = (p, t) match {
    case (Hole(change), _) => applyChange(change, t)
    case (LeafC(s1), Leaf(s2)) if s1 == s2 => Some(t)
    case (Node2C(p1, p2), Node2(t1, t2)) => for (
        t1_ <- applyPatch23(p1, t1);
        t2_ <- applyPatch23(p2, t2)
      ) yield Node2(t1_, t2_)
    case (Node3C(p1, p2, p3), Node3(t1, t2, t3)) => for (
        t1_ <- applyPatch23(p1, t1);
        t2_ <- applyPatch23(p2, t2);
        t3_ <- applyPatch23(p3, t3)
      ) yield Node3(t1_, t2_, t3_)
    case _ => None
  }
}
