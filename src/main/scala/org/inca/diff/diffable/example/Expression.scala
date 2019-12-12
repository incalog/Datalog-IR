package org.inca.diff.diffable.example

import org.inca.diff.diffable.DiffData.{Patch, VarMap}
import org.inca.diff.diffable.Diffable.{ApplyDiffFailed, DeletionFailedException, GreatestCommonPrefixFailed}
import org.inca.diff.diffable.{Diffable, DiffableOracle, Hole, MetaVar, Plug}

object Expression {

  trait Exp extends Diffable {
    override def _extract(oracle: DiffableOracle): Exp
    override def _retainHoles(vs: Set[_], orig: Patch): Exp
    override def _greatestCommonClosedPrefix(other: Patch): Exp
    override def _applyPatchToOrFail(p: Patch): Exp
    override def _insOrFail(m: VarMap): Exp
  }

  case class ExpHole(a: Plug) extends Exp with Hole[Exp] {
    override def mkHole: Plug => Exp = ExpHole.apply
  }

  case class Num(n: Int) extends Exp {
    lazy val $hash: Array[Byte] = {
      val digest = mkDigest
      digest.update(this.getClass.getCanonicalName.getBytes)
      hashNonDiffable(n, digest)
      digest.digest()
    }

    override val freevars: Set[MetaVar] = Set()
    override def visitDiffable(f: Diffable => Unit): Unit = f(this)

    override def _extract(oracle: DiffableOracle): Exp = oracle.predict(this) match {
      case Some(mv) => ExpHole(mv)
      case None => this
    }

    override def _retainHoles(vs: Set[_], orig: Diffable): Exp = this

    override def _greatestCommonClosedPrefix(other: Diffable): Exp = other match {
      case Num(n) if this.n == n => this
      case _ => mkPrefix(this, other, ExpHole.apply)
    }

    override def _applyPatchToOrFail(p: Patch): Exp = p match {
      case Num(n) if this.n == n => this
      case _ => throw ApplyDiffFailed()
    }

    override def _delOrFail(other: Diffable, m: VarMap): VarMap = other match {
      case Num(n) if this.n == n => m
      case _ => throw DeletionFailedException()
    }

    override def _insOrFail(m: VarMap): Exp = this
  }

  case class Add(e1: Exp, e2: Exp) extends Exp {
    lazy val $hash: Array[Byte] = {
      val digest = mkDigest
      digest.update(this.getClass.getCanonicalName.getBytes)
      digest.update(e1.$hash)
      digest.update(e2.$hash)
      digest.digest()
    }

    override def _extract(oracle: DiffableOracle): Exp = oracle.predict(this) match {
      case Some(mv) => ExpHole(mv)
      case None => Add(e1._extract(oracle), e2._extract(oracle))
    }

    override def _retainHoles(vs: Set[_], orig: Patch): Exp = orig match {
      case Add(e1, e2) => Add(this.e1._retainHoles(vs, e1), this.e2._retainHoles(vs, e2))
    }

    override def _greatestCommonClosedPrefix(other: Patch): Exp = other match {
      case Add(e1, e2) =>
        try {
          val p1 = this.e1._greatestCommonClosedPrefix(e1)
          val p2 = this.e2._greatestCommonClosedPrefix(e2)
          Add(p1, p2)
        } catch {
          case GreatestCommonPrefixFailed() => mkPrefix(this, other, ExpHole.apply)
        }
      case _ => mkPrefix(this, other, ExpHole.apply)
    }

    override def _applyPatchToOrFail(p: Patch): Exp = p match {
      case Add(e1, e2) => Add(this.e1._applyPatchToOrFail(e1), this.e2._applyPatchToOrFail(e2))
      case _ => throw ApplyDiffFailed()
    }

    override def _insOrFail(m: VarMap): Exp =
      Add(this.e1._insOrFail(m), this.e2._insOrFail(m))

    override val freevars: Set[MetaVar] = e1.freevars ++ e2.freevars

    override def visitDiffable(f: Patch => Unit): Unit = {
      f(this)
      this.e1.visitDiffable(f)
      this.e2.visitDiffable(f)
    }

    override def _delOrFail(other: Patch, m: VarMap): VarMap = other match {
      case Add(e1, e2) =>
        val m1 = this.e1._delOrFail(e1, m)
        val m2 = this.e2._delOrFail(e2, m1)
        m2
      case _ => throw DeletionFailedException()
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

    override def _extract(oracle: DiffableOracle): Exp = oracle.predict(this) match {
      case Some(mv) => ExpHole(mv)
      case None => Mul(e1._extract(oracle), e2._extract(oracle))
    }

    override def _retainHoles(vs: Set[_], orig: Patch): Exp = orig match {
      case Mul(e1, e2) => Mul(this.e1._retainHoles(vs, e1), this.e2._retainHoles(vs, e2))
    }

    override def _greatestCommonClosedPrefix(other: Patch): Exp = other match {
      case Mul(e1, e2) =>
        try {
          val p1 = this.e1._greatestCommonClosedPrefix(e1)
          val p2 = this.e2._greatestCommonClosedPrefix(e2)
          Mul(p1, p2)
        } catch {
          case GreatestCommonPrefixFailed() => mkPrefix(this, other, ExpHole.apply)
        }
      case _ => mkPrefix(this, other, ExpHole.apply)
    }

    override def _applyPatchToOrFail(p: Patch): Exp = p match {
      case Mul(e1, e2) => Mul(this.e1._applyPatchToOrFail(e1), this.e2._applyPatchToOrFail(e2))
      case _ => throw ApplyDiffFailed()
    }

    override def _insOrFail(m: VarMap): Exp =
      Mul(this.e1._insOrFail(m), this.e2._insOrFail(m))

    override val freevars: Set[MetaVar] = e1.freevars ++ e2.freevars

    override def visitDiffable(f: Patch => Unit): Unit = {
      f(this)
      this.e1.visitDiffable(f)
      this.e2.visitDiffable(f)
    }

    override def _delOrFail(other: Patch, m: VarMap): VarMap = other match {
      case Mul(e1, e2) =>
        val m1 = this.e1._delOrFail(e1, m)
        val m2 = this.e2._delOrFail(e2, m1)
        m2
      case _ => throw DeletionFailedException()
    }
  }

}
