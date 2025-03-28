package inca.ir.extension.not.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.extension.not.{Not, WeakNot}
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.visitors.IRVisitor
import sturdy.data.MayJoin

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]
  with IRVisitor: // we use the visitor to negate atoms

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    // Not and WeakNot only differ in their typing discipline
    case Not(atom) => evalAtomOpen(negateAtom(atom))
    case WeakNot(atom) => evalAtomOpen(negateAtom(atom))
    case _ => super.evalAtomOpen(at)