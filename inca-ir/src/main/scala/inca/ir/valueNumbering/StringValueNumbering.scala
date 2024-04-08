package inca.ir.valueNumbering
import inca.ir.extension.string.{StringLit,StringConcat,ToString}
import inca.ir.{Atom, Term}

trait StringValueNumbering(config: ConfigVN) extends BaseValueNumbering {

  protected override def getHashCode(term: Term): ValueId = term match {
    case StringLit(s) => s.hashCode()
    case StringConcat(lhs, rhs) => Seq(StringConcat,getHashCode(lhs),getHashCode(rhs)).hashCode()
    case ToString(t) => Seq(ToString,getHashCode(t)).hashCode()
    case _ => super.getHashCode(term)
  }

  protected override def isConst(term: Term): Boolean = term match {
    case StringLit(_) => true
    case _ => super.isConst(term)
  }

  protected override def removeAtomIfTrue(newAtomSeq: Seq[Atom]): Seq[Atom] = super.removeAtomIfTrue(newAtomSeq) // TODO include ?

  protected override def simplify(term: Term): Term = // TODO more cases? with ToString?
    if !this.config.simplifyArithmetic then return term // TODO move in BaseVN
    term match {
      case StringLit(s) => term
      case StringConcat(lhs, rhs) => (lhs,rhs) match {
        case (StringLit(s1), StringLit(s2)) => StringLit(s1.concat(s2))
        case _ => term
      }
      case ToString(t) => term
      case _ => super.simplify(term)
  }



}
