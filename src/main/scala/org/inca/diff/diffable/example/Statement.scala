package org.inca.diff.diffable.example

import org.inca.diff.HasCryptoHash
import org.inca.diff.diffable.DiffData.{Context, Patch, VarMap}
import org.inca.diff.diffable.Diffable.{ApplyDiffFailed, GreatestCommonPrefixFailed}
import org.inca.diff.diffable.{Change, ChangeHole, Diffable, DiffableOracle, MetaVar, MetaVarHole}

trait Stm extends Diffable[Stm]

case class StmVarHole(mv: MetaVar) extends Stm with MetaVarHole[Stm] {
  override def lifted: Stm = this
  override def mkChangeHole: Change[Stm] => Patch[Stm] = StmChangeHole.apply
}
case class StmChangeHole(change: Change[Stm]) extends Stm with ChangeHole[Stm] {
  override def lifted: Stm = this
}

//case class Assign(x: String, e: Exp) extends Stm {
//  override lazy val $hash: Array[Byte] = {
//    val digest = mkDigest
//    digest.update(this.getClass.getCanonicalName.getBytes())
//    hashNonDiffable(x, digest)
//    digest.update(e.$hash)
//    digest.digest()
//  }
//
//  override val freevars: Set[MetaVar] = e.freevars
//
//  override def initOracle(f: HasCryptoHash => Unit): Unit = {
//    f(this)
//    e.initOracle(f)
//  }
//
//  override def extract(oracle: DiffableOracle): Context[Stm] = oracle.predict(this) match {
//    case Some(mv) => StmVarHole(mv)
//    case None => Assign(x, e.extract(oracle))
//  }
//
//  override def retainMetaVars(vs: Set[MetaVar], orig: Context[Stm]): Context[Stm] = orig match {
//    case Assign(x, e) if this.x==x => Assign(x, this.e.retainMetaVars(vs, e))
//  }
//
//  override def greatestCommonClosedPrefix(other: Context[Stm]): Patch[Stm] = other match {
//    case Assign(x, e) if this.x==x =>
//      try {
//        val p = this.e.greatestCommonClosedPrefix(e)
//        Assign(x, p)
//      } catch {
//        case GreatestCommonPrefixFailed() => ChangeHole.mkClosedChangeHole(this, other, StmChangeHole.apply)
//      }
//    case _ => ChangeHole.mkClosedChangeHole(this, other, StmChangeHole.apply)
//  }
//
//  override def applyPatchTo(t: Stm): Stm = t match {
//    case Assign(x, e) if this.x==x => Assign(x, this.e.applyPatchTo(e))
//    case _ =>  throw ApplyDiffFailed()
//  }
//
//  override def del(other: Stm, m: VarMap[Stm]): VarMap[Stm] = other match {
//    case Assign(x, e) if this.x==x => this.e.del(e, m)
//  }
//
//  override def ins(m: VarMap[Stm]): Stm = Assign(x, e.ins(m))
//}


//case class While(cond: Exp, body: Stm) extends Stm
//case class Block(contents: List[Stm]) extends Stm