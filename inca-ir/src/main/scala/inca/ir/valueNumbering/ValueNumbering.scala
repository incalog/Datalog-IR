package inca.ir.valueNumbering

import inca.ir
import inca.ir.*

/*************************************************************************
 *  Assumptions:
 *   - no unsatisfiable atoms
 *   - no unbound Var (i.e. input was typechecked before)
 *
 *************************************************************************/

class ValueNumbering(config: ConfigVN = ConfigVN())
  extends BaseValueNumbering(config) 
    with ArithmeticValueNumbering(config)
    with StringValueNumbering(config) 

