package inca.ir.valueNumbering

import inca.ir
import inca.ir.*
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}

/*************************************************************************
 *  Assumptions:
 *   - no unsatisfiable atoms
 *   - no unbound Var (i.e. input was typechecked before)
 *
 *************************************************************************/

class ValueNumbering(typechecker: IRTypechecker = new IRTypechecker{})
  extends BaseValueNumbering(typechecker)
    with ArithmeticValueNumbering
    with StringValueNumbering
    with DataValueNumbering
