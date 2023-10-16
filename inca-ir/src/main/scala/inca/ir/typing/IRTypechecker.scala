package inca.ir.typing

import inca.ir.extension.*

class IRTypechecker extends BaseIRTypechecker
  with aggregation.Typechecker
  with arithmetic.Typechecker
  with block.Typechecker
  with bool.Typechecker
  with data.Typechecker
  with datamatch.Typechecker
  with demand.Typechecker
  with disjunction.Typechecker
  with not.Typechecker
  //with primitiveScala.Typechecker
  with set.Typechecker
  with string.Typechecker
  with tuple.Typechecker

