package inca.ir.valueNumbering

import inca.ir.valueNumbering.BaseVN.BaseValueNumbering
import inca.ir.valueNumbering.extensions.*

/*************************************************************************
 *  Assumptions:
 *   - no unbound Var (i.e. input was typechecked before)
 *   - only one module (other modules lowered before)
 *************************************************************************/

class ValueNumbering
  extends BaseValueNumbering
    with ArithmeticValueNumbering
    with StringValueNumbering
    with DataValueNumbering
    with AggregateValueNumbering 
