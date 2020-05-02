package org.inca.analyzedLangs

/**
 * As known from the thesis
 */


import org.inca.incer.IncrementalIndex

@IncrementalIndex
abstract class Exp extends CaseClassEqualityFix

@IncrementalIndex
abstract class Number(value: AnyVal) extends Exp

@IncrementalIndex
case class Integr(value: Int) extends Number

@IncrementalIndex
case class Decimal(value: Double) extends Number

@IncrementalIndex
case class Mul(lhs: Exp, rhs: Exp) extends Exp

@IncrementalIndex
case class Addi(lhs: Exp, rhs: Exp) extends Exp

@IncrementalIndex
case class Div(lhs: Exp, rhs: Exp) extends Exp
