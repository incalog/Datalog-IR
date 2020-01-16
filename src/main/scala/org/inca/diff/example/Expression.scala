package org.inca.diff.example

import org.inca.diff.DiffData.{Context, Patch}
import org.inca.diff.changeset.Changeset._
import org.inca.diff.{ApplyDiffFailed, GreatestCommonPrefixFailed, _}

import scala.collection.mutable.ArrayBuffer

trait Exp extends Diffable[Exp]

case class ExpVarHole(mv: MetaVar[Exp]) extends Exp with MetaVarHole[Exp] {
  override def lifted: Exp = this
  override def mkChangeHole: Change[Exp] => Patch[Exp] = ExpChangeHole.apply
}

case class ExpChangeHole(change: Change[Exp]) extends Exp with ChangeHole[Exp] {
  override def lifted: Exp = this
}

case class Num(n: Int) extends Exp {
  lazy val $hash: Array[Byte] = {
    val digest = mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes)
    hashNonDiffable(n, digest)
    digest.digest()
  }

  override val freevars: Set[MetaVar[_]] = Set()
  override def foreach(f: DiffableForeach): Unit = f(this)

  override def extract(oracle: DiffableOracle): Exp = oracle.predict[Exp](this) match {
    case Some(mv) => ExpVarHole(mv)
    case None => this
  }

  override def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[Exp]): Exp = this

  override def greatestCommonClosedPrefix(other: Context[Exp]): Patch[Exp] = other match {
    case Num(n) if this.n == n => this
    case _ => ChangeHole.mkClosedChangeHole(this, other, ExpChangeHole.apply)
  }

  override def findMinimalClosedChanges(other: Context[Exp], changes: ArrayBuffer[Change[_]]): Unit = other match {
    case Num(n) if this.n == n =>
    case _ => ChangeHole.addClosedChange(this, other, changes)
  }

  override def computeChangeset(parent: NodeRef, link: Link, other: Context[Exp], changes: ChangesetBuffer): Unit = other match {
    case Num(n) if this.n == n =>
    case _ =>
      this.unload(changes)
      changes += AttachNode(parent, link, other.load(changes, false))
  }

  override def load(changes: ChangesetBuffer, forceClone: Boolean): NodeRef = {
    val v = changes.freshVar()
    changes += LoadNode(v, this.getClass, Seq(
      NamedLink("n") -> Literal(n)
    ))
    v
  }


  override def unload(changes: ChangesetBuffer): Unit =
    changes += UnloadNode(this.ref)

  override def applyPatchTo(t: Exp): Exp = t match {
    case Num(n) if this.n == n => this
    case _ => throw ApplyDiffFailed()
  }

  override def matchTree(other: Exp): Unit = other match {
    case Num(n) if this.n == n =>
    case _ => throw ApplyDiffFailed()
  }

  override def buildTree(): Exp = this
}

case class Add(e1: Exp, e2: Exp) extends Exp {
  lazy val $hash: Array[Byte] = {
    val digest = mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes)
    digest.update(e1.$hash)
    digest.update(e2.$hash)
    digest.digest()
  }

  override def extract(oracle: DiffableOracle): Exp = oracle.predict[Exp](this) match {
    case Some(mv) => ExpVarHole(mv)
    case None => Add(e1.extract(oracle), e2.extract(oracle))
  }

  override def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[Exp]): Exp = orig match {
    case Add(e1, e2) => Add(this.e1.retainMetaVars(vs, e1), this.e2.retainMetaVars(vs, e2))
  }

  override def greatestCommonClosedPrefix(other: Context[Exp]): Patch[Exp] = other match {
    case Add(e1, e2) =>
      try {
        val p1 = this.e1.greatestCommonClosedPrefix(e1)
        val p2 = this.e2.greatestCommonClosedPrefix(e2)
        Add(p1, p2)
      } catch {
        case ex:GreatestCommonPrefixFailed => ChangeHole.mkClosedChangeHole(this, other, ExpChangeHole.apply, ex)
      }
    case _ => ChangeHole.mkClosedChangeHole(this, other, ExpChangeHole.apply)
  }

  override def findMinimalClosedChanges(other: Context[Exp], changes: ArrayBuffer[Change[_]]): Unit = other match {
    case Add(e1, e2) =>
      val changesBefore = changes.size
      try {
        this.e1.findMinimalClosedChanges(e1, changes)
        this.e2.findMinimalClosedChanges(e2, changes)
      } catch {
        case ex:GreatestCommonPrefixFailed =>
          changes.remove(changesBefore, changes.size - changesBefore)
          ChangeHole.addClosedChange(this, other, changes, ex)
      }
    case _ => ChangeHole.addClosedChange(this, other, changes)
  }

  override def computeChangeset(parent: NodeRef, link: Link, other: Context[Exp], changes: ChangesetBuffer): Unit = other match {
    case Add(e1, e2) =>
      this.e1.computeChangeset(this.ref, NamedLink("e1"), e1, changes)
      this.e2.computeChangeset(this.ref, NamedLink("e2"), e2, changes)
    case _ =>
      this.unload(changes)
      changes += AttachNode(parent, link, other.load(changes, false))
  }

  override def load(changes: ChangesetBuffer, forceClone: Boolean): NodeRef = {
    val ref1 = this.e1.load(changes, forceClone)
    val ref2 = this.e2.load(changes, forceClone)
    val v = changes.freshVar()
    changes += LoadNode(v, this.getClass, Seq(
      NamedLink("e1") -> ref1,
      NamedLink("e2") -> ref2
    ))
    v
  }

  override def unload(changes: ChangesetBuffer): Unit = {
    this.e1.unload(changes)
    this.e2.unload(changes)
    changes += UnloadNode(this.ref)
  }

  override def applyPatchTo(t: Exp): Exp = t match {
    case Add(e1, e2) => Add(this.e1.applyPatchTo(e1), this.e2.applyPatchTo(e2))
    case _ => throw ApplyDiffFailed()
  }

  override def buildTree(): Exp =
    Add(this.e1.buildTree(), this.e2.buildTree())

  override val freevars: Set[MetaVar[_]] = e1.freevars ++ e2.freevars

  override def foreach(f: DiffableForeach): Unit ={
    f(this)
    this.e1.foreach(f)
    this.e2.foreach(f)
  }

  override def matchTree(other: Exp): Unit = other match {
    case Add(e1, e2) => this.e1.matchTree(e1); this.e2.matchTree(e2)
    case _ => throw ApplyDiffFailed()
  }
}

case class Mul(e1: Exp, e2: Exp) extends Exp {
  lazy val $hash: Array[Byte] = {
    val digest = mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes)
    digest.update(e1.$hash)
    digest.update(e2.$hash)
    digest.digest()
  }

  override def extract(oracle: DiffableOracle): Exp = oracle.predict[Exp](this) match {
    case Some(mv) => ExpVarHole(mv)
    case None => Mul(e1.extract(oracle), e2.extract(oracle))
  }

  override def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[Exp]): Exp = orig match {
    case Mul(e1, e2) => Mul(this.e1.retainMetaVars(vs, e1), this.e2.retainMetaVars(vs, e2))
  }

  override def greatestCommonClosedPrefix(other: Context[Exp]): Patch[Exp] = other match {
    case Mul(e1, e2) =>
      try {
        val p1 = this.e1.greatestCommonClosedPrefix(e1)
        val p2 = this.e2.greatestCommonClosedPrefix(e2)
        Mul(p1, p2)
      } catch {
        case ex: GreatestCommonPrefixFailed => ChangeHole.mkClosedChangeHole(this, other, ExpChangeHole.apply, ex)
      }
    case _ => ChangeHole.mkClosedChangeHole(this, other, ExpChangeHole.apply)
  }

  override def findMinimalClosedChanges(other: Context[Exp], changes: ArrayBuffer[Change[_]]): Unit = other match {
    case Mul(e1, e2) =>
      val changesBefore = changes.size
      try {
        this.e1.findMinimalClosedChanges(e1, changes)
        this.e2.findMinimalClosedChanges(e2, changes)
      } catch {
        case ex:GreatestCommonPrefixFailed =>
          changes.remove(changesBefore, changes.size - changesBefore)
          ChangeHole.addClosedChange(this, other, changes, ex)
      }
    case _ => ChangeHole.addClosedChange(this, other, changes)
  }

  override def computeChangeset(parent: NodeRef, link: Link, other: Context[Exp], changes: ChangesetBuffer): Unit = other match {
    case Mul(e1, e2) =>
      this.e1.computeChangeset(this.ref, NamedLink("e1"), e1, changes)
      this.e2.computeChangeset(this.ref, NamedLink("e2"), e2, changes)
    case _ =>
      this.unload(changes)
      changes += AttachNode(parent, link, other.load(changes, false))
  }

  override def load(changes: ChangesetBuffer, forceClone: Boolean): NodeRef = {
    val ref1 = this.e1.load(changes, forceClone)
    val ref2 = this.e2.load(changes, forceClone)
    val v = changes.freshVar()
    changes += LoadNode(v, this.getClass, Seq(
      NamedLink("e1") -> ref1,
      NamedLink("e2") -> ref2
    ))
    v
  }

  override def unload(changes: ChangesetBuffer): Unit = {
    this.e1.unload(changes)
    this.e2.unload(changes)
    changes += UnloadNode(this.ref)
  }

  override def applyPatchTo(p: Exp): Exp = p match {
    case Mul(e1, e2) => Mul(this.e1.applyPatchTo(e1), this.e2.applyPatchTo(e2))
    case _ => throw ApplyDiffFailed()
  }

  override val freevars: Set[MetaVar[_]] = e1.freevars ++ e2.freevars

  override def foreach(f: DiffableForeach): Unit = {
    f(this)
    this.e1.foreach(f)
    this.e2.foreach(f)
  }

  override def matchTree(other: Exp): Unit = other match {
    case Mul(e1, e2) => this.e1.matchTree(e1); this.e2.matchTree(e2)
    case _ => throw ApplyDiffFailed()
  }

  override def buildTree(): Exp =
    Mul(this.e1.buildTree(), this.e2.buildTree())
}
