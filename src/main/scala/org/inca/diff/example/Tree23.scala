package org.inca.diff.example

import org.inca.diff.DiffData.{Context, Patch}
import org.inca.diff.{ApplyDiffFailed, GreatestCommonPrefixFailed}
import org.inca.diff._

trait Tree23 extends Diffable[Tree23]

case class Tree23MetaVarHole(mv: MetaVar[Tree23]) extends Tree23 with MetaVarHole[Tree23] {
  override def lifted: Context[Tree23] = this
  override def mkChangeHole: Change[Tree23] => Patch[Tree23] = Tree23ChangeHole.apply
}

case class Tree23ChangeHole(change: Change[Tree23]) extends Tree23 with ChangeHole[Tree23] {
  override def lifted: Tree23 = this
}

case class Leaf(s: String) extends Tree23 {
  override lazy val $hash: Array[Byte] = {
    val digest = mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes())
    hashNonDiffable(s, digest)
    digest.digest()
  }

  override lazy val freevars: Set[MetaVar[_]] =
    Set()

  override def extract(oracle: DiffableOracle): Tree23 = oracle.predict[Tree23](this) match {
    case Some(i) => Tree23MetaVarHole(i)
    case _ => this
  }

  override def foreach(f: DiffableForeach): Unit =
    f(this)

  override def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[Tree23]): this.type =
    this

  override def greatestCommonClosedPrefix(other: Context[Tree23]): Patch[Tree23] = other match {
    case Leaf(s) if this.s == s => this
    case _ => ChangeHole.mkClosedChangeHole(this, other, Tree23ChangeHole.apply)
  }

  override def applyPatchTo(t: Tree23): Tree23 = t match {
    case Leaf(s) if this.s == s => this
    case _ => throw ApplyDiffFailed()
  }

  override def buildTree(): this.type =
    this

  override def matchTree(other: Tree23): Unit = other match {
    case Leaf(s) if this.s == s =>
    case _ => throw ApplyDiffFailed()
  }

  override def size: Int = 0

  override def changeSize: Int = 1
}

case class Node2(t1: Tree23, t2: Tree23) extends Tree23 {
  override lazy val $hash: Array[Byte] = {
    val digest = mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes())
    digest.update(t1.$hash)
    digest.update(t2.$hash)
    digest.digest()
  }

  override lazy val freevars: Set[MetaVar[_]] =
    t1.freevars ++ t2.freevars

  override def extract(oracle: DiffableOracle): Tree23 = oracle.predict[Tree23](this) match {
    case Some(i) => Tree23MetaVarHole(i)
    case _ => Node2(t1.extract(oracle), t2.extract(oracle))
  }

  override def foreach(f: DiffableForeach): Unit = {
    f(this); t1.foreach(f); t2.foreach(f)
  }

  override def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[Tree23]): Tree23 = orig match {
    case Node2(t1, t2) => Node2(this.t1.retainMetaVars(vs, t1), this.t2.retainMetaVars(vs, t2))
  }

  override def greatestCommonClosedPrefix(other: Context[Tree23]): Patch[Tree23] = other match {
    case Node2(t1, t2) =>
      try {
        val p1 = this.t1.greatestCommonClosedPrefix(t1)
        val p2 = this.t2.greatestCommonClosedPrefix(t2)
        Node2(p1, p2)
      } catch {
        case ex: GreatestCommonPrefixFailed => ChangeHole.mkClosedChangeHole(this, other, Tree23ChangeHole.apply, ex)
    }
    case _ => ChangeHole.mkClosedChangeHole(this, other, Tree23ChangeHole.apply)
  }

  override def applyPatchTo(t: Tree23): Tree23 = t match {
    case Node2(t1, t2) => Node2(this.t1.applyPatchTo(t1), this.t2.applyPatchTo(t2))
    case _ => throw ApplyDiffFailed()
  }

  override def matchTree(other: Tree23): Unit = other match {
    case Node2(t1, t2) => this.t1.matchTree(t1); this.t2.matchTree(t2)
    case _ => throw ApplyDiffFailed()
  }

  override def buildTree(): Tree23 =
    Node2(t1.buildTree(), t2.buildTree())

  override def size: Int = t1.size + t2.size

  override def changeSize: Int = 1 + t1.changeSize + t2.changeSize
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

  override lazy val freevars: Set[MetaVar[_]] =
    t1.freevars ++ t2.freevars ++ t3.freevars

  override def extract(oracle: DiffableOracle): Tree23 = oracle.predict[Tree23](this) match {
    case Some(i) => Tree23MetaVarHole(i)
    case _ => Node3(t1.extract(oracle), t2.extract(oracle), t3.extract(oracle))
  }

  override def foreach(f: DiffableForeach): Unit = {
    f(this); t1.foreach(f); t2.foreach(f); t3.foreach(f)
  }

  override def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[Tree23]): Tree23 = orig match {
    case Node3(t1, t2, t3) =>
      Node3(
        this.t1.retainMetaVars(vs, t1),
        this.t2.retainMetaVars(vs, t2),
        this.t3.retainMetaVars(vs, t3))
  }

  override def greatestCommonClosedPrefix(other: Context[Tree23]): Patch[Tree23] = other match {
    case Node3(t1, t2, t3)=>
      try {
        val p1 = this.t1.greatestCommonClosedPrefix(t1)
        val p2 = this.t2.greatestCommonClosedPrefix(t2)
        val p3 = this.t3.greatestCommonClosedPrefix(t3)
        Node3(p1, p2, p3)
      } catch {
        case ex: GreatestCommonPrefixFailed => ChangeHole.mkClosedChangeHole(this, other, Tree23ChangeHole.apply, ex)
      }
    case _ => ChangeHole.mkClosedChangeHole(this, other, Tree23ChangeHole.apply)
  }

  override def applyPatchTo(t: Tree23): Tree23 = t match {
    case Node3(t1, t2, t3) => Node3(this.t1.applyPatchTo(t1), this.t2.applyPatchTo(t2), this.t3.applyPatchTo(t3))
    case _ => throw ApplyDiffFailed()
  }

  override def matchTree(other: Tree23): Unit = other match {
    case Node3(t1, t2, t3) => this.t1.matchTree(t1); this.t2.matchTree(t2); this.t3.matchTree(t3)
    case _ => throw ApplyDiffFailed()
  }

  override def buildTree(): Tree23 =
    Node3(t1.buildTree(), t2.buildTree(), t3.buildTree())

  override def size: Int = t1.size + t2.size + t3.size

  override def changeSize: Int = 1 + t1.changeSize + t2.changeSize + t3.changeSize
}
