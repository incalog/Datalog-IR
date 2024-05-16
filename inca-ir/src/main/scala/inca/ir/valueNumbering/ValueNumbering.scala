package inca.ir.valueNumbering

import inca.ir
import inca.ir.*

/*************************************************************************
 *  Assumptions:
 *   - no unsatisfiable atoms
 *   - no unbound Var (i.e. input was typechecked before)
 *
 *************************************************************************/

trait ValueNumbering 
  extends BaseValueNumbering 
    with ArithmeticValueNumbering
    with StringValueNumbering
