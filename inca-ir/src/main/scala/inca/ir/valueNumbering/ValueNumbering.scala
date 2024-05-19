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

class ValueNumbering(typechecker: BaseIRTypechecker = new IRTypechecker{}) // TODO okay? 
  extends BaseValueNumbering(typechecker)
    with ArithmeticValueNumbering
    with StringValueNumbering
