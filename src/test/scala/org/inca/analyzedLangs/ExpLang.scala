package org.inca.analyzedLangs

import org.inca.incer.IncrementalIndex
import org.inca.lang.Values._

@IncrementalIndex
trait Expression

@IncrementalIndex
case class BooleanLit(value: Boolean) extends Expression with BooleanLiteral

@IncrementalIndex
case class IntegerLit(value: Int) extends Expression with IntegerLiteral

@IncrementalIndex
case class LongLit(value: Long) extends  Expression with LongLiteral

@IncrementalIndex
case class Mult(lhs: Expression, rhs: Expression) extends Expression

@IncrementalIndex
case class Add(lhs: Expression, rhs: Expression) extends Expression

@IncrementalIndex
case class Not(e: Expression) extends Expression

@IncrementalIndex
case class And(lhs: Expression, rhs: Expression) extends Expression

@IncrementalIndex
case class Or(lhs: Expression, rhs: Expression) extends Expression

