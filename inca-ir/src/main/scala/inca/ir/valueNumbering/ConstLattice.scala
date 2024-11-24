package inca.ir.valueNumbering

import inca.ir.Term


enum ConstLattice {
  case Top
  case Const(l: Term)
  case Bot

  def join(that: ConstLattice): ConstLattice = (this, that) match {
    case (Const(l1), Const(l2)) if l1 == l2 => Const(l1)
    case (_, Bot) => this
    case (Bot, _) => that
    case _ => Top
  }

  def toOption: Option[Term] = this match {
    case Const(l) => Some(l)
    case _ => None
  }
  
  def isConst: Boolean = this match
    case Const(_) => true
    case _ => false 

}

object ConstLattice {
  def toConst(t: Term, isConst: Term => Boolean): ConstLattice = {
    if (isConst(t)) Const(t)
    else Top
  }
}
