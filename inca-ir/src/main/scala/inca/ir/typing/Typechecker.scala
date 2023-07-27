package inca.ir.typing

import inca.ir.extension.*

trait Typechecker extends BaseIRTypechecker
  with tuple.Typechecker
  with disjunction.Typechecker
  with block.Typechecker
  with arithmetic.Typechecker
  with data.Typechecker
  with not.Typechecker
  with set.Typechecker
  with primitiveScala.Typechecker

