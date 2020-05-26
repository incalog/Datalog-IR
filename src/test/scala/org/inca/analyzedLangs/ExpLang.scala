package org.inca.analyzedLangs

import org.inca.analyzedLangs.calcLang.Exp
import org.inca.incer.IncrementalIndex

@IncrementalIndex
abstract class Expression extends CaseClassEqualityFix

@IncrementalIndex
case class BooleanLit(value: Boolean) extends Exp

@IncrementalIndex
 case class IntegerLit(value: Int) extends Exp

@IncrementalIndex
case class LongLit(value: Long) extends  Exp

@IncrementalIndex
case class Mult(lhs: Exp, rhs: Exp) extends Exp

@IncrementalIndex
case class Add(lhs: Exp, rhs: Exp) extends Exp

@IncrementalIndex
case class Not(e: Exp) extends Exp

@IncrementalIndex
case class And(lhs: Exp, rhs: Exp) extends Exp

@IncrementalIndex
case class Or(lhs: Exp, rhs: Exp) extends Exp

