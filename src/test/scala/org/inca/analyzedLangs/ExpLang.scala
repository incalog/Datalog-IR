package org.inca.analyzedLangs

import org.inca.incer.IncrementalIndex

@IncrementalIndex
abstract class Expression extends CaseClassEqualityFix

@IncrementalIndex
case class BooleanLit(value: Boolean) extends Expression

@IncrementalIndex
 case class IntegerLit(value: Int) extends Expression

@IncrementalIndex
case class LongLit(value: Long) extends  Expression

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

