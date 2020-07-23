package inca.runtime

import truediff.Diffable
import truediff.macros.diffable

@diffable
trait Exp extends Diffable

@diffable
case class Num(n: Int) extends Exp { }

@diffable
case class Add(l: Exp, r: Exp) extends Exp { }

@diffable
case class Mul(l: Exp, r: Exp) extends Exp { }
