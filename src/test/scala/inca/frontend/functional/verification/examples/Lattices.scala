package inca.frontend.functional.verification.examples

import inca.compiler.Compiler
import inca.frontend.functional.compiler.{CompiledFunctionalModule, FunctionalOptions}

object Lattices {

  // CanDo: Extract lattices to .finca files

  val signLattice: String =
    s"""module SignLattice
       |data Sign = Top() | Bot() | Pos() | Zero() | Neg()
       |
       |@aggr(assoc, comm) def join(s1: Sign, s2: Sign): Sign = s1 match {
       |  case Top() => Top()
       |  case Bot() => s2
       |  case Pos() => s2 match {
       |    case Top() => Top()
       |    case Bot() => s1
       |    case Pos() => Pos()
       |    case Zero() => Top()
       |    case Neg() => Top()
       |  }
       |  case Zero() => s2 match {
       |    case Top() => Top()
       |    case Bot() => s1
       |    case Pos() => Top()
       |    case Zero() => Zero()
       |    case Neg() => Top()
       |  }
       |  case Neg() => s2 match {
       |    case Top() => Top()
       |    case Bot() => s1
       |    case Pos() => Top()
       |    case Zero() => Top()
       |    case Neg() => Neg()
       |  }
       |}
       |""".stripMargin

  val compiledSignLattice: CompiledFunctionalModule = Compiler.compileFunctional(signLattice, FunctionalOptions())

  val constLattice: String =
    s"""module ConstantLattice
       |data Constant = Bot() | Num(Int) | Top()
       |
       |@aggr(assoc, comm) def join(c1: Constant, c2: Constant): Constant = c1 match {
       |  case Top() => Top()
       |  case Bot() => c2
       |  case Num(i1) => c2 match {
       |    case Top() => Top()
       |    case Bot() => c1
       |    case Num(i2) => if (i1==i2) c1 else Top()
       |  }
       |}
       |""".stripMargin


  val compiledConstLattice: CompiledFunctionalModule = Compiler.compileFunctional(constLattice, FunctionalOptions())

  val signValLattice: String =
    s"""module SignValLattice
       |data Val = Top() | Bot() | BoolVal(Bool) | SignVal(Sign)
       |data Bool = TopBool() | BotBool() | True() | False()
       |data Sign = TopSign() | BotSign() | Pos() | Zero() | Neg()
       |
       |
       |@aggr(assoc, comm) def join(v1: Val, v2: Val): Val = v1 match {
       |  case Top() => Top()
       |  case Bot() => v2
       |  case BoolVal(b1) => v2 match {
       |    case Top() => Top()
       |    case Bot() => v1
       |    case BoolVal(b2) => BoolVal(joinBool(b1, b2))
       |    case SignVal(s) => Top()
       |  }
       |  case SignVal(s1) => v2 match {
       |    case Top() => Top()
       |    case Bot() => v1
       |    case BoolVal(b) => Top()
       |    case SignVal(s2) => SignVal(joinSign(s1, s2))
       |  }
       |}
       |
       |@aggr(assoc, comm) def joinBool(b1: Bool, b2: Bool): Bool = b1 match {
       |  case TopBool() => TopBool()
       |  case BotBool() => b2
       |  case True() => b2 match {
       |    case TopBool() => TopBool()
       |    case BotBool() => b1
       |    case True() => True()
       |    case False() => TopBool()
       |  }
       |  case False() => b2 match {
       |    case TopBool() => TopBool()
       |    case BotBool() => b1
       |    case True() => TopBool()
       |    case False() => False()
       |  }
       |}
       |
       |@aggr(assoc, comm) def joinSign(s1: Sign, s2: Sign): Sign = s1 match {
       |  case TopSign() => TopSign()
       |  case BotSign() => s2
       |  case Neg() => s2 match {
       |    case TopSign() => TopSign()
       |    case BotSign() => s1
       |    case Neg() => Neg()
       |    case Zero() => TopSign()
       |    case Pos() => TopSign()
       |  }
       |  case Zero() => s2 match {
       |    case TopSign() => TopSign()
       |    case BotSign() => s1
       |    case Neg() => TopSign()
       |    case Zero() => Zero()
       |    case Pos() => TopSign()
       |  }
       |  case Pos() => s2 match {
       |    case TopSign() => TopSign()
       |    case BotSign() => s1
       |    case Neg() => TopSign()
       |    case Zero() => TopSign()
       |    case Pos() => Pos()
       |  }
       |}
       |""".stripMargin

  val compiledSignValLattice: CompiledFunctionalModule = Compiler.compileFunctional(signValLattice, FunctionalOptions())

  val intervalLattice: String =
    """module IntervalLattice
      |data Interval = IV(Int, Int) | TopInterval()
      |data Bool = True() | False() | TopBool()
      |data Val = BotVal() | IntervalVal(Interval) | BoolVal(Bool) | TopVal()
      |
      |@aggr(assoc, comm) def joinVal(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => v2
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => v1
      |    case IntervalVal(iv2) => IntervalVal(joinInterval(iv1, iv2))
      |    case BoolVal(b2) => TopVal()
      |    case TopVal() => TopVal()
      |  }
      |  case BoolVal(b1) => v2 match {
      |    case BotVal() => v1
      |    case IntervalVal(iv2) => TopVal()
      |    case BoolVal(b2) => BoolVal(joinBool(b1, b2))
      |    case TopVal() => TopVal()
      |  }
      |  case TopVal() => TopVal()
      |}
      |@aggr(assoc, comm) def joinInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) => widenInterval(IV(min(l1, l2), max(h1, h2)))
      |  }
      |}
      |def widenInterval(iv: Interval): Interval = iv match {
      |  case TopInterval() => TopInterval()
      |  case IV(l, h) =>
      |    if (abs(h - l) <= 10)
      |      iv
      |    else
      |      TopInterval()
      |}
      |def min(i1: Int, i2: Int): Int = if(i1 < i2) i1 else i2
      |def max(i1: Int, i2: Int): Int = if(i1 < i2) i2 else i1
      |def abs(i: Int): Int = if(i < 0) i * (-1) else i
      |@aggr(assoc, comm) def joinBool(b1: Bool, b2: Bool): Bool = b1 match {
      |  case True() => b2 match {
      |    case True() => True()
      |    case False() => TopBool()
      |    case TopBool() => TopBool()
      |  }
      |  case False() => b2 match {
      |    case True() => TopBool()
      |    case False() => False()
      |    case TopBool() => TopBool()
      |  }
      |  case TopBool() => TopBool()
      |}""".stripMargin


  val compiledIntervalLattice: CompiledFunctionalModule = Compiler.compileFunctional(intervalLattice, FunctionalOptions())

  val intervalLatticeInvariants: String =
    """module IntervalLattice
      |@invariant(intervalBounds)
      |data Interval = IV(Int, Int) | TopInterval()
      |data Bool = True() | False() | TopBool()
      |@invariant(intervalBoundsVal)
      |data Val = BotVal() | IntervalVal(Interval) | BoolVal(Bool) | TopVal()
      |
      |def intervalBounds(iv: Interval): Boolean = iv match {
      |  case TopInterval() => true
      |  case IV(l, h) => if(l <= h) true else false
      |}
      |
      |def intervalBoundsVal(v: Val): Boolean = v match {
      |  case BotVal() => true
      |  case IntervalVal(iv) => intervalBounds(iv)
      |  case BoolVal(b) => true
      |  case TopVal() => true
      |}
      |
      |@aggr(assoc, comm) def joinVal(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => v2
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => v1
      |    case IntervalVal(iv2) => IntervalVal(joinInterval(iv1, iv2))
      |    case BoolVal(b2) => TopVal()
      |    case TopVal() => TopVal()
      |  }
      |  case BoolVal(b1) => v2 match {
      |    case BotVal() => v1
      |    case IntervalVal(iv2) => TopVal()
      |    case BoolVal(b2) => BoolVal(joinBool(b1, b2))
      |    case TopVal() => TopVal()
      |  }
      |  case TopVal() => TopVal()
      |}
      |@aggr(assoc, comm) def joinInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) => widenInterval(IV(min(l1, l2), max(h1, h2)))
      |  }
      |}
      |def widenInterval(iv: Interval): Interval = iv match {
      |  case TopInterval() => TopInterval()
      |  case IV(l, h) =>
      |    if (abs(h - l) <= 10)
      |      iv
      |    else
      |      TopInterval()
      |}
      |def min(i1: Int, i2: Int): Int = if(i1 < i2) i1 else i2
      |def max(i1: Int, i2: Int): Int = if(i1 < i2) i2 else i1
      |def abs(i: Int): Int = if(i < 0) i * (-1) else i
      |@aggr(assoc, comm) def joinBool(b1: Bool, b2: Bool): Bool = b1 match {
      |  case True() => b2 match {
      |    case True() => True()
      |    case False() => TopBool()
      |    case TopBool() => TopBool()
      |  }
      |  case False() => b2 match {
      |    case True() => TopBool()
      |    case False() => False()
      |    case TopBool() => TopBool()
      |  }
      |  case TopBool() => TopBool()
      |}""".stripMargin

  val compiledIntervalLatticeInvariants: CompiledFunctionalModule = Compiler.compileFunctional(intervalLatticeInvariants, FunctionalOptions())

  val boolLattice: String =
    """module BoolLattice
      |data Bool = True() | False() | TopBool()
      |
      |@aggr(assoc, comm) def joinBool(b1: Bool, b2: Bool): Bool = b1 match {
      |  case True() => b2 match {
      |    case True() => True()
      |    case False() => TopBool()
      |    case TopBool() => TopBool()
      |  }
      |  case False() => b2 match {
      |    case True() => TopBool()
      |    case False() => False()
      |    case TopBool() => TopBool()
      |  }
      |  case TopBool() => TopBool()
      |}
      |""".stripMargin

  val compiledBoolLattice: CompiledFunctionalModule = Compiler.compileFunctional(boolLattice, FunctionalOptions())

  val intOpsSignLattice: String =
    """module IntOpsSignLattice
      |data Sign = Top() | Bot() | Pos() | Zero() | Neg()
      |data Bool = BotBool() | True() | False() | TopBool()
      |
      |@aggr(assoc, comm, unapply(sub)) def add(i1: Int, i2: Int): Int = i1 + i2
      |@aggr(assoc, comm, unapply(add)) def sub(i1: Int, i2: Int): Int = i1 - i2
      |@aggr(assoc, comm) def mult(i1: Int, i2: Int): Int = i1 * i2
      |@aggr(assoc, comm) def div(i1: Int, i2: Int): Int = i1 / i2
      |def gt(i1: Int, i2: Int): Boolean = i1 > i2
      |def equals(i1: Int, i2: Int): Boolean = i1 == i2
      |
      |@sound(add, intToSign, intToSign, leqSign)
      |@monotone(leqSign, leqSign)
      |def addSign(s1: Sign, s2: Sign): Sign = s1 match {
      |  case Bot() => Bot()
      |  case Top() => s2 match {
      |    case Bot() => Bot()
      |    case Top() => Top()
      |    case Pos() => Top()
      |    case Zero() => Top()
      |    case Neg() => Top()
      |  }
      |  case Pos() => s2 match {
      |    case Bot() => Bot()
      |    case Top() => Top()
      |    case Pos() => Pos()
      |    case Zero() => Pos()
      |    case Neg() => Top()
      |  }
      |  case Zero() => s2 match {
      |    case Bot() => Bot()
      |    case Top() => Top()
      |    case Pos() => Pos()
      |    case Zero() => Zero()
      |    case Neg() => Neg()
      |  }
      |  case Neg() => s2 match {
      |    case Bot() => Bot()
      |    case Top() => Top()
      |    case Pos() => Top()
      |    case Zero() => Neg()
      |    case Neg() => Neg()
      |  }
      |}
      |
      |@sound(sub, intToSign, intToSign, leqSign)
      |@monotone(leqSign, leqSign)
      |def subSign(s1: Sign, s2: Sign): Sign = s1 match {
      |  case Bot() => Bot()
      |  case Top() => s2 match {
      |    case Bot() => Bot()
      |    case Top() => Top()
      |    case Pos() => Top()
      |    case Zero() => Top()
      |    case Neg() => Top()
      |  }
      |  case Pos() => s2 match {
      |    case Bot() => Bot()
      |    case Top() => Top()
      |    case Pos() => Top()
      |    case Zero() => Pos()
      |    case Neg() => Pos()
      |  }
      |  case Zero() => s2 match {
      |    case Bot() => Bot()
      |    case Top() => Top()
      |    case Pos() => Neg()
      |    case Zero() => Zero()
      |    case Neg() => Pos()
      |  }
      |  case Neg() => s2 match {
      |    case Bot() => Bot()
      |    case Top() => Top()
      |    case Pos() => Neg()
      |    case Zero() => Neg()
      |    case Neg() => Top()
      |  }
      |}
      |
      |@sound(mult, intToSign, intToSign, leqSign)
      |@monotone(leqSign, leqSign)
      |def multSign(s1: Sign, s2: Sign): Sign = s1 match {
      |  case Bot() => Bot()
      |  case Zero() => s2 match {
      |    case Bot() => Bot()
      |    case Zero() => Zero()
      |    case Top() => Zero()
      |    case Pos() => Zero()
      |    case Neg() => Zero()
      |  }
      |  case Top() => s2 match {
      |    case Bot() => Bot()
      |    case Zero() => Zero()
      |    case Top() => Top()
      |    case Pos() => Top()
      |    case Neg() => Top()
      |  }
      |  case Pos() => s2 match {
      |    case Bot() => Bot()
      |    case Zero() => Zero()
      |    case Top() => Top()
      |    case Pos() => Pos()
      |    case Neg() => Neg()
      |  }
      |  case Neg() => s2 match {
      |    case Bot() => Bot()
      |    case Zero() => Zero()
      |    case Top() => Top()
      |    case Pos() => Neg()
      |    case Neg() => Pos()
      |  }
      |}
      |
      |@sound(div, intToSign, intToSign, leqSign)
      |@monotone(leqSign, leqSign)
      |def divSign(s1: Sign, s2: Sign): Sign = s1 match {
      |  case Bot() => Bot()
      |  case Top() => s2 match {
      |    case Bot() => Bot()
      |    case Zero() => Bot()
      |    case Top() => Top()
      |    case Pos() => Top()
      |    case Neg() => Top()
      |  }
      |  case Zero() => s2 match {
      |    case Bot() => Bot()
      |    case Zero() => Bot()
      |    case Top() => Top()
      |    case Pos() => Zero()
      |    case Neg() => Zero()
      |  }
      |  case Pos() => s2 match {
      |    case Bot() => Bot()
      |    case Zero() => Bot()
      |    case Top() => Top()
      |    case Pos() => Top()
      |    case Neg() => Top()
      |  }
      |  case Neg() => s2 match {
      |    case Bot() => Bot()
      |    case Zero() => Bot()
      |    case Top() => Top()
      |    case Pos() => Top()
      |    case Neg() => Top()
      |  }
      |}
      |
      |@sound(gt, intToSign, booleanToBool, leqBool)
      |@monotone(leqSign, leqBool)
      |def gtSign(s1: Sign, s2: Sign): Bool = s1 match {
      |  case Bot() => BotBool()
      |  case Zero() => s2 match {
      |    case Bot() => BotBool()
      |    case Zero() => False()
      |    case Top() => TopBool()
      |    case Pos() => False()
      |    case Neg() => True()
      |  }
      |  case Top() => s2 match {
      |    case Bot() => BotBool()
      |    case Zero() => TopBool()
      |    case Top() => TopBool()
      |    case Pos() => TopBool()
      |    case Neg() => TopBool()
      |  }
      |  case Pos() => s2 match {
      |    case Bot() => BotBool()
      |    case Zero() => True()
      |    case Top() => TopBool()
      |    case Pos() => TopBool()
      |    case Neg() => True()
      |  }
      |  case Neg() => s2 match {
      |    case Bot() => BotBool()
      |    case Zero() => False()
      |    case Top() => TopBool()
      |    case Pos() => False()
      |    case Neg() => TopBool()
      |  }
      |}
      |
      |@sound(equals, intToSign, booleanToBool, leqBool)
      |@monotone(leqSign, leqBool)
      |def equalsSign(s1: Sign, s2: Sign): Bool = s1 match {
      |  case Bot() => BotBool()
      |  case Zero() => s2 match {
      |    case Bot() => BotBool()
      |    case Zero() => TopBool()
      |    case Top() => TopBool()
      |    case Pos() => False()
      |    case Neg() => False()
      |  }
      |  case Top() => s2 match {
      |    case Bot() => BotBool()
      |    case Zero() => TopBool()
      |    case Top() => TopBool()
      |    case Pos() => TopBool()
      |    case Neg() => TopBool()
      |  }
      |  case Pos() => s2 match {
      |    case Bot() => BotBool()
      |    case Zero() => False()
      |    case Top() => TopBool()
      |    case Pos() => TopBool()
      |    case Neg() => False()
      |  }
      |  case Neg() => s2 match {
      |    case Bot() => BotBool()
      |    case Zero() => False()
      |    case Top() => TopBool()
      |    case Pos() => False()
      |    case Neg() => TopBool()
      |  }
      |}
      |
      |@partialOrder def leqSign(s1: Sign, s2: Sign): Boolean = s1 match {
      |  case Top() => s2 match {
      |    case Top() => true
      |    case Bot() => false
      |    case Pos() => false
      |    case Zero() => false
      |    case Neg() => false
      |  }
      |  case Bot() => true
      |  case Pos() => s2 match {
      |    case Top() => true
      |    case Bot() => false
      |    case Pos() => true
      |    case Zero() => false
      |    case Neg() => false
      |  }
      |  case Zero() => s2 match {
      |    case Top() => true
      |    case Bot() => false
      |    case Pos() => false
      |    case Zero() => true
      |    case Neg() => false
      |  }
      |  case Neg() => s2 match {
      |    case Top() => true
      |    case Bot() => false
      |    case Pos() => false
      |    case Zero() => false
      |    case Neg() => true
      |  }
      |}
      |
      |@partialOrder def leqBool(b1: Bool, b2: Bool): Boolean = b1 match {
      |  case TopBool() => b2 match {
      |    case TopBool() => true
      |    case True() => false
      |    case False() => false
      |    case BotBool() => false
      |  }
      |  case BotBool() => true
      |  case True() => b2 match {
      |    case TopBool() => true
      |    case BotBool() => false
      |    case True() => true
      |    case False() => false
      |  }
      |  case False() => b2 match {
      |    case TopBool() => true
      |    case BotBool() => false
      |    case True() => false
      |    case False() => true
      |  }
      |}
      |
      |def intToSign(i: Int): Sign =
      |  if (i == 0) Zero() else
      |    if (i > 0) Pos() else Neg()
      |
      |def booleanToBool(b: Boolean): Bool =
      |  if(b) True() else False()
      |
      |""".stripMargin

  val compiledIntOpsSignLattice: CompiledFunctionalModule = Compiler.compileFunctional(intOpsSignLattice, FunctionalOptions())

  val doubleOpsConstantLattice: String =
    """module DoubleOpsConstantPropagationLattice
      |data Constant = Bot() | Num(Double) | Top()
      |data Bool = BotBool() | True() | False() | TopBool()
      |
      |@aggr(assoc, comm, unapply(sub)) def add(d1: Double, d2: Double): Double = d1 + d2
      |@aggr(assoc, comm, unapply(add)) def sub(d1: Double, d2: Double): Double = d1 - d2
      |@aggr(assoc, comm) def mult(d1: Double, d2: Double): Double = d1 * d2
      |@aggr(assoc, comm) def div(d1: Double, d2: Double): Double = d1 / d2
      |def gt(d1: Double, d2: Double): Boolean = d1 > d2
      |def equals(d1: Double, d2: Double): Boolean = d1 == d2
      |
      |@sound(add, doubleToConst, doubleToConst, leqConst)
      |@monotone(leqConst, leqConst)
      |def addConst(c1: Constant, c2: Constant): Constant = c1 match {
      |  case Bot() => Bot()
      |  case Top() => Top()
      |  case Num(d1) => c2 match {
      |    case Bot() => Bot()
      |    case Top() => Top()
      |    case Num(d2) => Num(d1 + d2)
      |  }
      |}
      |
      |@sound(sub, doubleToConst, doubleToConst, leqConst)
      |@monotone(leqConst, leqConst)
      |def subConst(c1: Constant, c2: Constant): Constant = c1 match {
      |  case Bot() => Bot()
      |  case Top() => Top()
      |  case Num(d1) => c2 match {
      |    case Bot() => Bot()
      |    case Top() => Top()
      |    case Num(d2) => Num(d1 - d2)
      |  }
      |}
      |
      |@sound(mult, doubleToConst, doubleToConst, leqConst)
      |@monotone(leqConst, leqConst)
      |def multConst(c1: Constant, c2: Constant): Constant = c1 match {
      |  case Bot() => Bot()
      |  case Top() => Top()
      |  case Num(d1) => c2 match {
      |    case Bot() => Bot()
      |    case Top() => Top()
      |    case Num(d2) => Num(d1 * d2)
      |  }
      |}
      |
      |@sound(div, doubleToConst, doubleToConst, leqConst)
      |@monotone(leqConst, leqConst)
      |def divConst(c1: Constant, c2: Constant): Constant = c1 match {
      |  case Bot() => Bot()
      |  case Top() => Top()
      |  case Num(d1) => c2 match {
      |    case Bot() => Bot()
      |    case Top() => Top()
      |    case Num(d2) => Num(d1 / d2)
      |  }
      |}
      |
      |@sound(gt, doubleToConst, booleanToBool, leqBool)
      |@monotone(leqConst, leqBool)
      |def gtConst(c1: Constant, c2: Constant): Bool = c1 match {
      |  case Bot() => BotBool()
      |  case Top() => TopBool()
      |  case Num(d1) => c2 match {
      |    case Bot() => BotBool()
      |    case Top() => TopBool()
      |    case Num(d2) => if(d1 > d2) True() else False()
      |  }
      |}
      |
      |@sound(equals, doubleToConst, booleanToBool, leqBool)
      |@monotone(leqConst, leqBool)
      |def equalsConst(c1: Constant, c2: Constant): Bool = c1 match {
      |  case Bot() => BotBool()
      |  case Top() => TopBool()
      |  case Num(d1) => c2 match {
      |    case Bot() => BotBool()
      |    case Top() => TopBool()
      |    case Num(d2) => if(d1 == d2) True() else False()
      |  }
      |}
      |
      |@partialOrder def leqConst(c1: Constant,c2: Constant): Boolean = c1 match {
      |  case Top() => c2 match {
      |    case Top() => true
      |    case Bot() => false
      |    case Num(d2) => false
      |  }
      |  case Bot() => true
      |  case Num(d1) => c2 match {
      |    case Top() => true
      |    case Bot() => false
      |    case Num(d2) => if (d1==d2) true else false
      |  }
      |}
      |
      |@partialOrder def leqBool(b1: Bool, b2: Bool): Boolean = b1 match {
      |  case TopBool() => b2 match {
      |    case TopBool() => true
      |    case True() => false
      |    case False() => false
      |    case BotBool() => false
      |  }
      |  case BotBool() => true
      |  case True() => b2 match {
      |    case TopBool() => true
      |    case BotBool() => false
      |    case True() => true
      |    case False() => false
      |  }
      |  case False() => b2 match {
      |    case TopBool() => true
      |    case BotBool() => false
      |    case True() => false
      |    case False() => true
      |  }
      |}
      |
      |def doubleToConst(d: Double): Constant = Num(d)
      |
      |def booleanToBool(b: Boolean): Bool =
      |  if(b) True() else False()
      |
      |""".stripMargin

  val compiledDoubleOpsConstantLattice: CompiledFunctionalModule = Compiler.compileFunctional(doubleOpsConstantLattice, FunctionalOptions())

  val intervalLatticeOps: String =
    """module IntervalLatticeOps
      |@invariant(intervalBounds)
      |data Interval = IV(Int, Int) | TopInterval()
      |data Bool = True() | False() | TopBool()
      |@invariant(intervalBoundsVal)
      |data Val = BotVal() | IntervalVal(Interval) | BoolVal(Bool) | TopVal()
      |
      |def intervalBounds(iv: Interval): Boolean = iv match {
      |  case TopInterval() => true
      |  case IV(l, h) => if(l <= h) true else false
      |}
      |
      |def intervalBoundsVal(v: Val): Boolean = v match {
      |  case BotVal() => true
      |  case IntervalVal(iv) => intervalBounds(iv)
      |  case BoolVal(b) => true
      |  case TopVal() => true
      |}
      |
      |def add(i1: Int, i2: Int): Int = i1 + i2
      |def sub(i1: Int, i2: Int): Int = i1 - i2
      |def mul(i1: Int, i2: Int): Int = i1 * i2
      |def gt(i1: Int, i2: Int): Boolean = i1 > i2
      |
      |@aggr(assoc, comm)
      |@monotone(leqVal, leqVal)
      |def joinVal(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => v2
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => v1
      |    case IntervalVal(iv2) => IntervalVal(joinInterval(iv1, iv2))
      |    case BoolVal(b2) => TopVal()
      |    case TopVal() => TopVal()
      |  }
      |  case BoolVal(b1) => v2 match {
      |    case BotVal() => v1
      |    case IntervalVal(iv2) => TopVal()
      |    case BoolVal(b2) => BoolVal(joinBool(b1, b2))
      |    case TopVal() => TopVal()
      |  }
      |  case TopVal() => TopVal()
      |}
      |
      |@aggr(assoc, comm)
      |@monotone(leqInterval, leqInterval)
      |def joinInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) => widenInterval(IV(min(l1, l2), max(h1, h2)))
      |  }
      |}
      |
      |@monotone(leqInterval, leqInterval)
      |def widenInterval(iv: Interval): Interval = iv match {
      |  case TopInterval() => TopInterval()
      |  case IV(l, h) =>
      |    if (abs(h - l) <= 10)
      |      iv
      |    else
      |      TopInterval()
      |}
      |
      |@aggr(assoc, comm)
      |@monotone(leqBool, leqBool)
      |def joinBool(b1: Bool, b2: Bool): Bool = b1 match {
      |  case True() => b2 match {
      |    case True() => True()
      |    case False() => TopBool()
      |    case TopBool() => TopBool()
      |  }
      |  case False() => b2 match {
      |    case True() => TopBool()
      |    case False() => False()
      |    case TopBool() => TopBool()
      |  }
      |  case TopBool() => TopBool()
      |}
      |
      |@monotone(leqVal, leqVal)
      |@sound(gt, intToVal, booleanToVal, leqVal)
      |def greaterThan(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => BotVal()
      |  case BoolVal(b1) => BotVal()
      |  case TopVal() => BoolVal(TopBool())
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => BoolVal(TopBool())
      |    case IntervalVal(iv2) => BoolVal(greaterThanInterval(iv1, iv2))
      |  }
      |}
      |
      |@monotone(leqInterval, leqBool)
      |def greaterThanInterval(iv1: Interval, iv2: Interval): Bool = iv1 match {
      |  case TopInterval() => TopBool()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopBool()
      |    case IV(l2, h2) =>
      |      if (l1 > h2) True()
      |      else if (l2 > h1) False()
      |      else TopBool()
      |  }
      |}
      |
      |@aggr(assoc, comm)
      |@sound(add, intToVal, intToVal, leqVal)
      |@monotone(leqVal, leqVal)
      |def addVal(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => BotVal()
      |  case BoolVal(b1) => BotVal()
      |  case TopVal() => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => IntervalVal(TopInterval())
      |    case IntervalVal(iv2) => IntervalVal(TopInterval())
      |  }
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => IntervalVal(TopInterval())
      |    case IntervalVal(iv2) => IntervalVal(addInterval(iv1, iv2))
      |  }
      |}
      |
      |@aggr(assoc, comm)
      |@monotone(leqInterval, leqInterval)
      |def addInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) => IV(l1 + l2, h1 + h2)
      |  }
      |}
      |
      |@aggr(assoc, comm)
      |@sound(sub, intToVal, intToVal, leqVal)
      |@monotone(leqVal, leqVal)
      |def subVal(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => BotVal()
      |  case BoolVal(b1) => BotVal()
      |  case TopVal() => IntervalVal(TopInterval())
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => IntervalVal(TopInterval())
      |    case IntervalVal(iv2) => IntervalVal(subInterval(iv1, iv2))
      |  }
      |}
      |
      |@aggr(assoc, comm)
      |@monotone(leqInterval, leqInterval)
      |def subInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) => IV(l1 - h2, h1 - l2)
      |  }
      |}
      |
      |@aggr(assoc, comm)
      |@sound(mul, intToVal, intToVal, leqVal)
      |@monotone(leqVal, leqVal)
      |def mulVal(v1: Val, v2: Val): Val = v1 match {
      |  case BotVal() => BotVal()
      |  case BoolVal(b1) => BotVal()
      |  case TopVal() => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => IntervalVal(TopInterval())
      |    case IntervalVal(iv2) => IntervalVal(TopInterval())
      |  }
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => BotVal()
      |    case BoolVal(b2) => BotVal()
      |    case TopVal() => IntervalVal(TopInterval())
      |    case IntervalVal(iv2) => IntervalVal(mulInterval(iv1, iv2))
      |  }
      |}
      |
      |@aggr(assoc, comm)
      |@monotone(leqInterval, leqInterval)
      |def mulInterval(iv1: Interval, iv2: Interval): Interval = iv1 match {
      |  case TopInterval() => TopInterval()
      |  case IV(l1, h1) => iv2 match {
      |    case TopInterval() => TopInterval()
      |    case IV(l2, h2) =>
      |      let v1 = l1 * l2 in
      |      let v2 = l1 * h2 in
      |      let v3 = h1 * l2 in
      |      let v4 = h1 * h2 in
      |      let low = min(v1, min(v2, min(v3, v4))) in
      |      let high = max(v1, max(v2, max(v3, v4))) in
      |      IV(low, high)
      |  }
      |}
      |
      |@partialOrder def leqInterval(iv1: Interval, iv2: Interval): Boolean = iv1 match {
      |  case TopInterval() => iv2 match {
      |    case TopInterval() => true
      |    case IV(l2, u2) => false
      |  }
      |  case IV(l1, u1) => iv2 match {
      |    case TopInterval() => true
      |    case IV(l2, u2) => if((l1 >= l2) && (u1 <= u2)) true else false
      |  }
      |}
      |
      |@partialOrder def leqBool(b1: Bool, b2: Bool): Boolean = b1 match {
      |  case TopBool() => b2 match {
      |    case TopBool() => true
      |    case True() => false
      |    case False() => false
      |  }
      |  case True() => b2 match {
      |    case TopBool() => true
      |    case True() => true
      |    case False() => false
      |  }
      |  case False() => b2 match {
      |    case TopBool() => true
      |    case True() => false
      |    case False() => true
      |  }
      |}
      |
      |@partialOrder def leqVal(v1: Val, v2: Val): Boolean = v1 match {
      |  case BotVal() => true
      |  case IntervalVal(iv1) => v2 match {
      |    case BotVal() => false
      |    case IntervalVal(iv2) => leqInterval(iv1, iv2)
      |    case BoolVal(b2) => false
      |    case TopVal() => true
      |  }
      |  case BoolVal(b1) => v2 match {
      |    case BotVal() => false
      |    case IntervalVal(iv2) => false
      |    case BoolVal(b2) => leqBool(b1, b2)
      |    case TopVal() => true
      |  }
      |  case TopVal() => v2 match {
      |    case BotVal() => false
      |    case IntervalVal(iv2) => false
      |    case BoolVal(b2) => false
      |    case TopVal() => true
      |  }
      |}
      |
      |def min(i1: Int, i2: Int): Int = if(i1 < i2) i1 else i2
      |def max(i1: Int, i2: Int): Int = if(i1 < i2) i2 else i1
      |def abs(i: Int): Int = if(i < 0) i * (-1) else i
      |
      |def intToVal(i: Int): Val = IntervalVal(IV(i, i))
      |def booleanToVal(b: Boolean): Val = if(b) BoolVal(True()) else BoolVal(False())
      |""".stripMargin

  val compiledIntervalLatticeOps: CompiledFunctionalModule = Compiler.compileFunctional(intervalLatticeOps, FunctionalOptions())
}