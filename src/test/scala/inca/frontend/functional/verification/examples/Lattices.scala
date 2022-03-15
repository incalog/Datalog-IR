package inca.frontend.functional.verification.examples

import inca.frontend.functional.core._
import inca.frontend.functional.parser.Parser
import inca.compiler.Compiler
import inca.frontend.functional.compiler.FunctionalOptions

object Lattices {

  val signLattice =
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

  //val signLatticeModule: Module = Parser.parse(signLattice)

  val compiledSignLattice = Compiler.compileFunctional(signLattice, FunctionalOptions())

  val constLattice =
    s"""module ConstantPropagationLattice
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


  val compiledConstLattice = Compiler.compileFunctional(constLattice, FunctionalOptions())

  val signValLattice =
    s"""module SignValLattice
       |data Val = Top() | Bot() | BoolVal(Boole) | SignVal(Sign)
       |data Boole = TopBool() | BotBool() | True() | False()
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
       |@aggr(assoc, comm) def joinBool(b1: Boole, b2: Boole): Boole = b1 match {
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

  val compiledSignValLattice = Compiler.compileFunctional(signValLattice, FunctionalOptions())

  val intervalLattice =
    """module IntervalLattice
      |data Interval = IV(Int, Int) | TopInterval()
      |data Boole = True() | False() | TopBool()
      |data Val = BotVal() | IntervalVal(Interval) | BoolVal(Boole) | TopVal()
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
      |@aggr(assoc, comm) def joinBool(b1: Boole, b2: Boole): Boole = b1 match {
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


  val compiledIntervalLattice = Compiler.compileFunctional(intervalLattice, FunctionalOptions())

  val modifiedIntervalLattice =
    """module IntervalLattice
      |data Interval = IV(Int, Int) | TopInterval()
      |data Boole = True() | False() | TopBool()
      |data Val = BotVal() | IntervalVal(Interval) | BoolVal(Boole) | TopVal()
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
      |    case IV(l2, h2) => if((l1 <= h1) && (l2 <= h2)) widenInterval(IV(min(l1, l2), max(h1, h2))) else TopInterval()
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
      |@aggr(assoc, comm) def joinBool(b1: Boole, b2: Boole): Boole = b1 match {
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

  val compiledModifiedIntervalLattice = Compiler.compileFunctional(modifiedIntervalLattice, FunctionalOptions())

  // funktioniert nicht, da Bool ein protected word ist in z3
  val boolLattice =
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

  val compiledBoolLattice = Compiler.compileFunctional(boolLattice, FunctionalOptions())
}