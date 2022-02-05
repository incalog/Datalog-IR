package inca.frontend.functional.verification.examples

import inca.frontend.functional.core._
import inca.frontend.functional.parser.Parser

object Lattices {

  val sign_lattice =
    s"""module SignLattice
       |data Sign = Neg() | Zero() | Pos() | Bot() | Top()
       |
       |@main def join(s1: Sign, s2: Sign): Sign = s1 match {
       |  case Top() => Top()
       |  case Bot() => s2
       |  case Neg() => s2 match {
       |    case Top() => Top()
       |    case Bot() => s1
       |    case Neg() => Neg()
       |    case Zero() => Top()
       |    case Pos() => Top()
       |  }
       |  case Zero() => s2 match {
       |    case Top() => Top()
       |    case Bot() => s1
       |    case Neg() => Top()
       |    case Zero() => Zero()
       |    case Pos() => Top()
       |  }
       |  case Zero() => s2 match {
       |    case Top() => Top()
       |    case Bot() => s1
       |    case Neg() => Top()
       |    case Zero() => Top()
       |    case Pos() => Pos()
       |  }
       |}
       |""".stripMargin

  val sign_lattice_module: Module = Parser.parse(sign_lattice)

  val const_lattice =
    s"""module ConstantPropagationLattice
       |data Constant = Bot() | Num(Int) | Top()
       |
       |@main def join(c1: Constant, c2: Constant): Constant = c1 match {
       |  case Top() => Top()
       |  case Bot() => c2
       |  case Num(i1) => c2 match {
       |    case Top() => Top()
       |    case Bot() => c1
       |    case Num(i2) => (if (i1==i2) {Num(i1)} else {Top()})
       |  }
       |}
       |""".stripMargin

  val const_lattice_module: Module = Parser.parse(const_lattice)
}
