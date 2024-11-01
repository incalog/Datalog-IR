package inca.ir.valueNumbering

import inca.ir
import inca.ir.*

/*************************************************************************
 *  Assumptions:
 *   - no unbound Var (i.e. input was typechecked before)
 *
 *************************************************************************/

class ValueNumbering
  extends BaseValueNumbering
    with ArithmeticValueNumbering
    with StringValueNumbering
    with DataValueNumbering
    with AggregateValueNumbering 
