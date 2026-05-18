package inca.ir.typing

import inca.ir.extension.*

class IRTypechecker extends BaseIRTypechecker
  with aggregate.Typechecker
  with aggregateset.Typechecker
  with aggregategeneric.Typechecker
  with arithmetic.Typechecker
  with block.Typechecker
  with bool.Typechecker
  with data.Typechecker
  with datamatch.Typechecker
  with demand.Typechecker
  with disjunction.Typechecker
  with foreign.Typechecker
  with impure.Typechecker
  with not.Typechecker
  with set.Typechecker
  with map.Typechecker
  with string.Typechecker
  with tuple.Typechecker
  with mono.Typechecker
  with typeparam.Typechecker
  with edbdata.Typechecker
  with record.Typechecker
  with list.Typechecker
  with locals.Typechecker

