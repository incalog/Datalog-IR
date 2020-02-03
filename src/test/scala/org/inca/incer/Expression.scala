package org.inca.incer

@IncrementalIndex
trait Exp

@IncrementalIndex
case class Num(n: Int) extends Exp { }

@IncrementalIndex
case class Add(l: Exp, r: Exp) extends Exp { }

@IncrementalIndex
case class Mul(l: Exp, r: Exp) extends Exp { }
