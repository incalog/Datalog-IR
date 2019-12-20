package org.inca.diff.diffable.example

import org.inca.diff.HasCryptoHash
import org.inca.diff.diffable.DiffData.{Context, Patch, VarMap}
import org.inca.diff.diffable.Diffable.{ApplyDiffFailed, GreatestCommonPrefixFailed}
import org.inca.diff.diffable._

trait Tree23 extends Diffable[Tree23]

case class Tree23MetaVarHole(mv: MetaVar) extends Tree23 with MetaVarHole[Tree23] {
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

  override lazy val freevars: Set[MetaVar] =
    Set()

  override def extract(oracle: DiffableOracle): Tree23 = oracle.predict(this) match {
    case Some(i) => example.Tree23MetaVarHole(i)
    case _ => this
  }

  override def initOracle(f: HasCryptoHash => Unit): Unit =
    f(this)

  override def retainMetaVars(vs: Set[MetaVar], orig: Context[Tree23]): this.type =
    this

  override def greatestCommonClosedPrefix(other: Context[Tree23]): Patch[Tree23] = other match {
    case Leaf(s) if this.s == s => this
    case _ => ChangeHole.mkClosedChangeHole(this, other, Tree23ChangeHole.apply)
  }

  override def applyPatchTo(t: Tree23): Tree23 = t match {
    case Leaf(s) if this.s == s => this
    case _ => throw ApplyDiffFailed()
  }

  override def ins(m: VarMap[Tree23]): this.type =
    this

  override def del(other: Tree23, m: VarMap[Tree23]): VarMap[Tree23] = other match {
    case Leaf(s) if this.s == s => m
    case _ => throw ApplyDiffFailed()
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

  override def extract(oracle: DiffableOracle): Tree23 = oracle.predict(this) match {
    case Some(i) => example.Tree23MetaVarHole(i)
    case _ => Node2(t1.extract(oracle), t2.extract(oracle))
  }

  override def initOracle(f: HasCryptoHash => Unit): Unit = {
    f(this); t1.initOracle(f); t2.initOracle(f)
  }

  override def retainMetaVars(vs: Set[MetaVar], orig: Context[Tree23]): Tree23 = orig match {
    case Node2(t1, t2) => Node2(this.t1.retainMetaVars(vs, t1), this.t2.retainMetaVars(vs, t2))
  }

  override def greatestCommonClosedPrefix(other: Context[Tree23]): Patch[Tree23] = other match {
    case Node2(t1, t2) =>
      try {
        val p1 = this.t1.greatestCommonClosedPrefix(t1)
        val p2 = this.t2.greatestCommonClosedPrefix(t2)
        Node2(p1, p2)
      } catch {
        case GreatestCommonPrefixFailed() => ChangeHole.mkClosedChangeHole(this, other, Tree23ChangeHole.apply)
    }
    case _ => ChangeHole.mkClosedChangeHole(this, other, Tree23ChangeHole.apply)
  }

  override def applyPatchTo(t: Tree23): Tree23 = t match {
    case Node2(t1, t2) => Node2(this.t1.applyPatchTo(t1), this.t2.applyPatchTo(t2))
    case _ => throw ApplyDiffFailed()
  }

  override def del(other: Tree23, m: VarMap[Tree23]): VarMap[Tree23] = other match {
    case Node2(t1, t2) => this.t2.del(t2, this.t1.del(t1, m))
    case _ => throw ApplyDiffFailed()
  }

  override def ins(m: VarMap[Tree23]): Tree23 =
    Node2(t1.ins(m), t2.ins(m))
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

  override def extract(oracle: DiffableOracle): Tree23 = oracle.predict(this) match {
    case Some(i) => example.Tree23MetaVarHole(i)
    case _ => Node3(t1.extract(oracle), t2.extract(oracle), t3.extract(oracle))
  }

  override def initOracle(f: HasCryptoHash => Unit): Unit = {
    f(this); t1.initOracle(f); t2.initOracle(f); t3.initOracle(f)
  }

  override def retainMetaVars(vs: Set[MetaVar], orig: Context[Tree23]): Tree23 = orig match {
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
        case GreatestCommonPrefixFailed() => ChangeHole.mkClosedChangeHole(this, other, Tree23ChangeHole.apply)
      }
    case _ => ChangeHole.mkClosedChangeHole(this, other, Tree23ChangeHole.apply)
  }

  override def applyPatchTo(t: Tree23): Tree23 = t match {
    case Node3(t1, t2, t3) => Node3(this.t1.applyPatchTo(t1), this.t2.applyPatchTo(t2), this.t3.applyPatchTo(t3))
    case _ => throw ApplyDiffFailed()
  }

  override def del(other: Tree23, m: VarMap[Tree23]): VarMap[Tree23] = other match {
    case Node3(t1, t2, t3) => this.t3.del(t3, this.t2.del(t2, this.t1.del(t1, m)))
    case _ => throw ApplyDiffFailed()
  }

  override def ins(m: VarMap[Tree23]): Tree23 =
    Node3(t1.ins(m), t2.ins(m), t3.ins(m))
}
