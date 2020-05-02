package org.inca.analyzedLangs

/**
 * As known from the thesis
 */


import org.inca.incer.IncrementalIndex

@IncrementalIndex
abstract class Expression extends CaseClassEqualityFix

@IncrementalIndex
case class Number(value: Integer) extends Expression

@IncrementalIndex
case class Decimal(value: Double) extends Expression

@IncrementalIndex
case class Mult(lhs: Expression, rhs: Expression) extends Expression

@IncrementalIndex
case class Add(lhs: Expression, rhs: Expression) extends Expression

@IncrementalIndex
case class Div(lhs: Expression, rhs: Expression) extends Expression
