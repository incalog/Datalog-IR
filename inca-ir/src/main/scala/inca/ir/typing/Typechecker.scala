package inca.ir.typing

import inca.ir.extension.disjunction
import inca.ir.extension.tuple
import inca.ir.extension.block
import inca.ir.extension.arithmetic
import inca.ir.extension.bool
import inca.ir.extension.not

trait Typechecker extends BaseIRTypechecker
  with tuple.Typechecker
  with disjunction.Typechecker
  with block.Typechecker
  with arithmetic.Typechecker
  with not.Typechecker
