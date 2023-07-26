package inca.ir.typing

import inca.ir.extension.*

trait Typechecker extends BaseIRTypechecker
  with tuple.Typechecker
  with disjunction.Typechecker
  with block.Typechecker
  with arithmetic.Typechecker
  with not.Typechecker
  with data.Typechecker
  with primitiveScala.Typechecker

