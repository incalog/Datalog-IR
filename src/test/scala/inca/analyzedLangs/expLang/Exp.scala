package inca.analyzedLangs.expLang

import truediff.Diffable
import truediff.macros.diffable

@diffable
trait Exp extends Diffable
@diffable
case class BooleanLit(value: Boolean) extends Exp
@diffable
case class IntegerLit(value: Int) extends Exp
@diffable
case class LongLit(value: Long) extends  Exp
@diffable
case class Mult(lhs: Exp, rhs: Exp) extends Exp
@diffable
case class Add(lhs: Exp, rhs: Exp) extends Exp
@diffable
case class Not(e: Exp) extends Exp
@diffable
case class And(lhs: Exp, rhs: Exp) extends Exp
@diffable
case class Or(lhs: Exp, rhs: Exp) extends Exp

