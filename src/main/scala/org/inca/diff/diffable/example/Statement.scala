package org.inca.diff.diffable.example

import org.inca.diff.HasCryptoHash
import org.inca.diff.diffable.DiffData.{Context, Patch, VarMap}
import org.inca.diff.diffable.Diffable.{ApplyDiffFailed, GreatestCommonPrefixFailed}
import org.inca.diff.diffable.{Change, ChangeHole, Diffable, DiffableForeach, DiffableOracle, MetaVar, MetaVarHole}

trait Stm extends Diffable[Stm]

case class StmVarHole(mv: MetaVar[Stm]) extends Stm with MetaVarHole[Stm] {
  override def lifted: Stm = this
  override def mkChangeHole: Change[Stm] => Patch[Stm] = StmChangeHole.apply
}
case class StmChangeHole(change: Change[Stm]) extends Stm with ChangeHole[Stm] {
  override def lifted: Stm = this
}

case class Assign(x: String, e: Exp) extends Stm {
  override lazy val $hash: Array[Byte] = {
    val digest = mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes())
    hashNonDiffable(x, digest)
    digest.update(e.$hash)
    digest.digest()
  }

  override val freevars: Set[MetaVar[_]] = e.freevars

  override def foreach(f: DiffableForeach): Unit = {
    f(this)
    e.foreach(f)
  }

  override def extract(oracle: DiffableOracle): Context[Stm] = oracle.predict[Stm](this) match {
    case Some(mv) => StmVarHole(mv)
    case None => Assign(x, e.extract(oracle))
  }

  override def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[Stm]): Context[Stm] = orig match {
    case Assign(x, e) if this.x==x => Assign(x, this.e.retainMetaVars(vs, e))
  }

  override def greatestCommonClosedPrefix(other: Context[Stm]): Patch[Stm] = other match {
    case Assign(x, e) if this.x==x =>
      try {
        val p = this.e.greatestCommonClosedPrefix(e)
        Assign(x, p)
      } catch {
        case GreatestCommonPrefixFailed() => ChangeHole.mkClosedChangeHole(this, other, StmChangeHole.apply)
      }
    case _ => ChangeHole.mkClosedChangeHole(this, other, StmChangeHole.apply)
  }

  override def applyPatchTo(t: Stm): Stm = t match {
    case Assign(x, e) if this.x==x => Assign(x, this.e.applyPatchTo(e))
    case _ =>  throw ApplyDiffFailed()
  }

  override def matchTree(other: Stm): Unit = other match {
    case Assign(x, e) if this.x==x => this.e.matchTree(e)
  }

  override def buildTree(): Stm = Assign(x, e.buildTree())
}

case class While(cond: Exp, body: Stm) extends Stm {
  override lazy val $hash: Array[Byte] = {
    val digest = mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes())
    digest.update(cond.$hash)
    digest.update(body.$hash)
    digest.digest()
  }

  override val freevars: Set[MetaVar[_]] = cond.freevars ++ body.freevars

  override def foreach(f: DiffableForeach): Unit = {
    f(this)
    cond.foreach(f)
    body.foreach(f)
  }

  override def extract(oracle: DiffableOracle): Stm = oracle.predict[Stm](this) match {
    case Some(mv) => StmVarHole(mv)
    case None =>While(cond.extract(oracle), body.extract(oracle))
  }

  override def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[Stm]): Stm = orig match {
    case While(cond, body) => While(this.cond.retainMetaVars(vs, cond), this.body.retainMetaVars(vs, body))
  }

  override def greatestCommonClosedPrefix(other: Context[Stm]): Patch[Stm] = other match {
    case While(cond, body) =>
      try {
        val p1 = this.cond.greatestCommonClosedPrefix(cond)
        val p2 = this.body.greatestCommonClosedPrefix(body)
        While(p1, p2)
      } catch {
        case GreatestCommonPrefixFailed() => ChangeHole.mkClosedChangeHole(this, other, StmChangeHole.apply)
      }
    case _ => ChangeHole.mkClosedChangeHole(this, other, StmChangeHole.apply)
  }

  override def applyPatchTo(t: Stm): Stm = t match {
    case While(cond, body) => While(this.cond.applyPatchTo(cond), this.body.applyPatchTo(body))
    case _ => throw ApplyDiffFailed()
  }

  override def matchTree(other: Stm): Unit = other match {
    case While(cond, body) =>
      this.cond.matchTree(cond)
      this.body.matchTree(body)
    case _ => ApplyDiffFailed()
  }

  override def buildTree(): Stm = While(cond.buildTree(), body.buildTree())
}

case class Block(contents: List[Stm]) extends Stm {
  override lazy val $hash: Array[Byte] = {
    val digest = mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes())
    contents.foreach(s => digest.update(s.$hash))
    digest.digest()
  }

  override val freevars: Set[MetaVar[_]] = contents.foldLeft(Set[MetaVar[_]]())((vs, s) => vs union (s.freevars))

  override def foreach(f: DiffableForeach): Unit = {
    f(this)
    contents.foreach(_.foreach(f))
  }

  override def extract(oracle: DiffableOracle): Context[Stm] = oracle.predict[Stm](this) match {
    case Some(mv) => StmVarHole(mv)
    case None => Block(contents.map(_.extract(oracle)))
  }

  override def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[Stm]): Context[Stm] = orig match {
    case Block(contents) => Block(this.contents.zip(contents).map(p => p._1.retainMetaVars(vs, p._2)))
  }

  override def greatestCommonClosedPrefix(other: Context[Stm]): Patch[Stm] = other match {
    case Block(contents) =>
      try {
        val ps = this.contents.zip(contents).map(p => p._1.greatestCommonClosedPrefix(p._2))
        Block(ps)
      } catch {
        case GreatestCommonPrefixFailed() => ChangeHole.mkClosedChangeHole(this, other, StmChangeHole.apply)
      }
    case _ => ChangeHole.mkClosedChangeHole(this, other, StmChangeHole.apply)
  }

  override def applyPatchTo(t: Stm): Stm = t match {
    case Block(contents) => Block(this.contents.zip(contents).map(p => p._1.applyPatchTo(p._2)))
    case _ => throw ApplyDiffFailed()
  }

  override def matchTree(other: Stm): Unit = other match {
    case Block(contents) => this.contents.zip(contents).foreach(p => p._1.matchTree(p._2))
  }

  override def buildTree(): Stm = Block(contents.map(_.buildTree()))
}