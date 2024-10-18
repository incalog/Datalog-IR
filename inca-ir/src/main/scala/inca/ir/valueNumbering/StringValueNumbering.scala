package inca.ir.valueNumbering

import inca.ir.extension.string.{StringLit,StringConcat,ToString}
import inca.ir.{Atom, Term}


trait StringValueNumbering extends BaseValueNumbering {
  
  protected override def isConst(term: Term): Boolean = term match {
    case StringLit(_) => true
    case _ => super.isConst(term)
  }
  
  protected override def normalize(term: Term): Term =
    if !this.normalize then return term
    term match {
      case StringLit(s) => term
      case StringConcat(lhs, rhs) => (lhs,rhs) match {
        case (StringLit(s1), StringLit(s2)) => StringLit(s1.concat(s2))
        case _ => term
      }
      case ToString(t) => term // should only contain Int or Double; behaves like Scala .toString
      case _ => super.normalize(term)
  }
  
}
