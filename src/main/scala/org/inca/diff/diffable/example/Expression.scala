package org.inca.diff.diffable.example

import org.inca.diff.HasCryptoHash
import org.inca.diff.diffable.DiffData.{Context, Patch, VarMap}
import org.inca.diff.diffable.Diffable.{ApplyDiffFailed, GreatestCommonPrefixFailed}
import org.inca.diff.diffable._

trait Exp extends Diffable[Exp]

case class ExpVarHole(mv: MetaVar) extends Exp with MetaVarHole[Exp] {
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

  override val freevars: Set[MetaVar] = Set()
  override def initOracle(f: HasCryptoHash => Unit): Unit = f(this)

  override def extract(oracle: DiffableOracle): Exp = oracle.predict(this) match {
    case Some(mv) => ExpVarHole(mv)
    case None => this
  }

  override def retainMetaVars(vs: Set[MetaVar], orig: Context[Exp]): Exp = this

  override def greatestCommonClosedPrefix(other: Context[Exp]): Patch[Exp] = other match {
    case Num(n) if this.n == n => this
    case _ => ChangeHole.mkClosedChangeHole(this, other, ExpChangeHole.apply)
  }

  override def applyPatchTo(t: Exp): Exp = t match {
    case Num(n) if this.n == n => this
    case _ => throw ApplyDiffFailed()
  }

  override def del(other: Exp, m: VarMap[Exp]): VarMap[Exp] = other match {
    case Num(n) if this.n == n => m
    case _ => throw ApplyDiffFailed()
  }

  override def ins(m: VarMap[Exp]): Exp = this
}

case class Add(e1: Exp, e2: Exp) extends Exp {
  lazy val $hash: Array[Byte] = {
    val digest = mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes)
    digest.update(e1.$hash)
    digest.update(e2.$hash)
    digest.digest()
  }

  override def extract(oracle: DiffableOracle): Exp = oracle.predict(this) match {
    case Some(mv) => ExpVarHole(mv)
    case None => Add(e1.extract(oracle), e2.extract(oracle))
  }

  override def retainMetaVars(vs: Set[MetaVar], orig: Context[Exp]): Exp = orig match {
    case Add(e1, e2) => Add(this.e1.retainMetaVars(vs, e1), this.e2.retainMetaVars(vs, e2))
  }

  override def greatestCommonClosedPrefix(other: Context[Exp]): Patch[Exp] = other match {
    case Add(e1, e2) =>
      try {
        val p1 = this.e1.greatestCommonClosedPrefix(e1)
        val p2 = this.e2.greatestCommonClosedPrefix(e2)
        Add(p1, p2)
      } catch {
        case GreatestCommonPrefixFailed() => ChangeHole.mkClosedChangeHole(this, other, ExpChangeHole.apply)
      }
    case _ => ChangeHole.mkClosedChangeHole(this, other, ExpChangeHole.apply)
  }

  override def applyPatchTo(p: Exp): Exp = p match {
    case Add(e1, e2) => Add(this.e1.applyPatchTo(e1), this.e2.applyPatchTo(e2))
    case _ => throw ApplyDiffFailed()
  }

  override def ins(m: VarMap[Exp]): Exp =
    Add(this.e1.ins(m), this.e2.ins(m))

  override val freevars: Set[MetaVar] = e1.freevars ++ e2.freevars

  override def initOracle(f: HasCryptoHash => Unit): Unit ={
    f(this)
    this.e1.initOracle(f)
    this.e2.initOracle(f)
  }

  override def del(other: Exp, m: VarMap[Exp]): VarMap[Exp] = other match {
    case Add(e1, e2) =>
      val m1 = this.e1.del(e1, m)
      val m2 = this.e2.del(e2, m1)
      m2
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

  override def extract(oracle: DiffableOracle): Exp = oracle.predict(this) match {
    case Some(mv) => ExpVarHole(mv)
    case None => Mul(e1.extract(oracle), e2.extract(oracle))
  }

  override def retainMetaVars(vs: Set[MetaVar], orig: Context[Exp]): Exp = orig match {
    case Mul(e1, e2) => Mul(this.e1.retainMetaVars(vs, e1), this.e2.retainMetaVars(vs, e2))
  }

  override def greatestCommonClosedPrefix(other: Context[Exp]): Patch[Exp] = other match {
    case Mul(e1, e2) =>
      try {
        val p1 = this.e1.greatestCommonClosedPrefix(e1)
        val p2 = this.e2.greatestCommonClosedPrefix(e2)
        Mul(p1, p2)
      } catch {
        case GreatestCommonPrefixFailed() => ChangeHole.mkClosedChangeHole(this, other, ExpChangeHole.apply)
      }
    case _ => ChangeHole.mkClosedChangeHole(this, other, ExpChangeHole.apply)
  }

  override def applyPatchTo(p: Exp): Exp = p match {
    case Mul(e1, e2) => Mul(this.e1.applyPatchTo(e1), this.e2.applyPatchTo(e2))
    case _ => throw ApplyDiffFailed()
  }

  override def ins(m: VarMap[Exp]): Exp =
    Mul(this.e1.ins(m), this.e2.ins(m))

  override val freevars: Set[MetaVar] = e1.freevars ++ e2.freevars

  override def initOracle(f: HasCryptoHash => Unit): Unit = {
    f(this)
    this.e1.initOracle(f)
    this.e2.initOracle(f)
  }

  override def del(other: Exp, m: VarMap[Exp]): VarMap[Exp] = other match {
    case Mul(e1, e2) =>
      val m1 = this.e1.del(e1, m)
      val m2 = this.e2.del(e2, m1)
      m2
    case _ => throw ApplyDiffFailed()
  }
}
