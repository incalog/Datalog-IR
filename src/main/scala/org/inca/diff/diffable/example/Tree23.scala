package org.inca.diff.diffable.example

import org.inca.diff.diffable.DiffData.{Patch, VarMap}
import org.inca.diff.diffable.Diffable.{ApplyDiffFailed, DeletionFailedException, GreatestCommonPrefixFailed, InsertionFailedException}
import org.inca.diff.diffable._

trait Tree23 extends Diffable {
  override def _extract(oracle: DiffableOracle): Tree23
  override def _retainHoles(vs: Set[_], orig: Diffable): Tree23
  override def _greatestCommonClosedPrefix(other: Diffable): Tree23
  override def _applyPatchToOrFail(p: Patch): Tree23
  override def _insOrFail(m: VarMap): Tree23
}

case class Leaf(s: String) extends Tree23 {
  override lazy val $hash: Array[Byte] = {
    val digest = mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes())
    hashNonDiffable(s, digest)
    digest.digest()
  }

  override lazy val freevars: Set[MetaVar] =
    Set()

  override def _extract(oracle: DiffableOracle): Tree23 = oracle.predict(this) match {
    case Some(i) => example.Tree23Hole(i)
    case _ => this
  }

  override def visitDiffable(f: Diffable => Unit): Unit =
    f(this)

  override def _retainHoles(vs: Set[_], orig: Diffable): this.type =
    this

  override def _greatestCommonClosedPrefix(other: Diffable): Tree23 = other match {
    case Leaf(s) if this.s == s => this
    case _ => mkPrefix(this, other, Tree23Hole.apply)
  }

  override def _applyPatchToOrFail(p: Patch): Tree23 = p match {
    case Leaf(s) if this.s == s => this
    case _ => throw ApplyDiffFailed()
  }

  override def _insOrFail(m: VarMap): this.type =
    this

  override def _delOrFail(other: Diffable, m: VarMap): VarMap = other match {
    case Leaf(s) if this.s == s => m
    case _ => throw DeletionFailedException()
  }
}

case class Node2(t1: Tree23, t2: Tree23) extends Tree23 {
  override lazy val $hash: Array[Byte] = {
    val digest = mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes())
    digest.update(t1.$hash)
    digest.update(t2.$hash)
    digest.digest()
  }

  override lazy val freevars: Set[MetaVar] =
    t1.freevars ++ t2.freevars

  override def _extract(oracle: DiffableOracle): Tree23 = oracle.predict(this) match {
    case Some(i) => example.Tree23Hole(i)
    case _ => Node2(t1._extract(oracle), t2._extract(oracle))
  }

  override def visitDiffable(f: Diffable => Unit): Unit = {
    f(this); t1.visitDiffable(f); t2.visitDiffable(f)
  }

  override def _retainHoles(vs: Set[_], orig: Diffable): Tree23 = {
    val other = orig.asInstanceOf[Node2]
    Node2(t1._retainHoles(vs, other.t1), t2._retainHoles(vs, other.t2))
  }

  override def _greatestCommonClosedPrefix(other: Diffable): Tree23 = other match {
    case Node2(t1, t2) =>
      try {
        val p1 = this.t1._greatestCommonClosedPrefix(t1)
        val p2 = this.t2._greatestCommonClosedPrefix(t2)
        Node2(p1, p2)
      } catch {
        case GreatestCommonPrefixFailed() => mkPrefix(this, other, Tree23Hole.apply)
    }
    case _ => mkPrefix(this, other, Tree23Hole.apply)
  }

  override def _applyPatchToOrFail(p: Patch): Tree23 = p match {
    case Node2(t1, t2) => Node2(this.t1._applyPatchToOrFail(t1), this.t2._applyPatchToOrFail(t2))
    case _ => throw ApplyDiffFailed()
  }

  override def _delOrFail(other: Diffable, m: VarMap): VarMap = other match {
    case Node2(t1, t2) => this.t2._delOrFail(t2, this.t1._delOrFail(t1, m))
    case _ => throw DeletionFailedException()
  }

  override def _insOrFail(m: VarMap): Tree23 =
    Node2(t1._insOrFail(m), t2._insOrFail(m))
}

case class Node3(t1: Tree23, t2: Tree23, t3: Tree23) extends Tree23 {
  override lazy val $hash: Array[Byte] = {
    val digest = mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes())
    digest.update(t1.$hash)
    digest.update(t2.$hash)
    digest.update(t3.$hash)
    digest.digest()
  }

  override lazy val freevars: Set[MetaVar] =
    t1.freevars ++ t2.freevars ++ t3.freevars

  override def _extract(oracle: DiffableOracle): Tree23 = oracle.predict(this) match {
    case Some(i) => example.Tree23Hole(i)
    case _ => Node3(t1._extract(oracle), t2._extract(oracle), t3._extract(oracle))
  }

  override def visitDiffable(f: Diffable => Unit): Unit = {
    f(this); t1.visitDiffable(f); t2.visitDiffable(f); t3.visitDiffable(f)
  }

  override def _retainHoles(vs: Set[_], orig: Diffable): Tree23 = {
    val other = orig.asInstanceOf[Node3]
    Node3(t1._retainHoles(vs, other.t1), t2._retainHoles(vs, other.t2), t3._retainHoles(vs, other.t3))
  }

  override def _greatestCommonClosedPrefix(other: Diffable): Tree23 = other match {
    case Node3(t1, t2, t3)=>
      try {
        val p1 = this.t1._greatestCommonClosedPrefix(t1)
        val p2 = this.t2._greatestCommonClosedPrefix(t2)
        val p3 = this.t3._greatestCommonClosedPrefix(t3)
        Node3(p1, p2, p3)
      } catch {
        case GreatestCommonPrefixFailed() => mkPrefix(this, other, Tree23Hole.apply)
      }
    case _ => mkPrefix(this, other, Tree23Hole.apply)
  }

  override def _applyPatchToOrFail(p: Patch): Tree23 = p match {
    case Node3(t1, t2, t3) => Node3(this.t1._applyPatchToOrFail(t1), this.t2._applyPatchToOrFail(t2), this.t3._applyPatchToOrFail(t3))
    case _ => throw ApplyDiffFailed()
  }

  override def _delOrFail(other: Diffable, m: VarMap): VarMap = other match {
    case Node3(t1, t2, t3) => this.t3._delOrFail(t3, this.t2._delOrFail(t2, this.t1._delOrFail(t1, m)))
    case _ => throw DeletionFailedException()
  }

  override def _insOrFail(m: VarMap): Tree23 =
    Node3(t1._insOrFail(m), t2._insOrFail(m), t3._insOrFail(m))
}

case class Tree23Hole(a: Plug) extends Tree23 with Hole[Tree23] {
  override def mkHole: Plug => Tree23 = Tree23Hole.apply
}
