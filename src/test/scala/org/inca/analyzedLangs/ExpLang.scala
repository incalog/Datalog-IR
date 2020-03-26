package org.inca.analyzedLangs

import org.inca.incer.IncrementalIndex

@IncrementalIndex
trait Expression

@IncrementalIndex
case class BooleanLit(v: Boolean) extends Expression

@IncrementalIndex
case class IntegerLit(v: Int) extends Expression

@IncrementalIndex
case class LongLit(v: Long) extends Expression

@IncrementalIndex
case class Add(lhs: Expression, rhs: Expression) extends Expression

@IncrementalIndex
case class Mult(lhs: Expression, rhs: Expression) extends Expression

@IncrementalIndex
case class Not(e: Expression) extends Expression

@IncrementalIndex
case class And(lhs: Expression, rhs: Expression) extends Expression

@IncrementalIndex
case class Or(lhs: Expression, rhs: Expression) extends Expression

