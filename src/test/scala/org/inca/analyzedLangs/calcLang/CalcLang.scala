package org.inca.analyzedLangs.calcLang

/**
 * As known from the thesis
 */


import org.inca.analyzedLangs.CaseClassEqualityFix
import org.inca.incer.IncrementalIndex

@IncrementalIndex
abstract class Exp extends CaseClassEqualityFix

@IncrementalIndex
case class Integr(value: Int) extends Exp

@IncrementalIndex
case class Decimal(value: Double) extends Exp

@IncrementalIndex
case class Mul(lhs: Exp, rhs: Exp) extends Exp

@IncrementalIndex
case class Add(lhs: Exp, rhs: Exp) extends Exp

@IncrementalIndex
case class Div(lhs: Exp, rhs: Exp) extends Exp