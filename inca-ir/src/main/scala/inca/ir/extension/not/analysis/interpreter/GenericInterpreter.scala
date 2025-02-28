package inca.ir.extension.not.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.extension.not.{Not, WeakNot}
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.visitors.IRVisitor
import sturdy.data.MayJoin

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]
  with IRVisitor: // we use the visitor to negate atoms

  /*
  // This naive approach does not work.
  // E.g. consider a filter
  //  Q(x) :- x == 1 v x == 2 x == 3
  //  R(x) :- Q(x), Not(x == 1).
  // If we use the naive approach below, then we reset the supplementary to the state
  // before the filtering! However, what we want to do is negate the filter from == to !=.
  // This naive approach would only work for existential queries.

  private def negateAtom(at: Atom)(using rec: Fixed): Unit =
    val snapshot = snapshotSupplementary()
    except.tryCatch {
      evalAtom(at)
      // produce an empty supplementary
      updateSupplementaryChecked { sup =>
        val cols = relationOps.columns(sup)
        relationOps.make(cols, Seq())
      }
    } /* catch */ { _ =>
      // negation does not bind => rollback the changes to the supplementary
      updateSupplementaryUnchecked { _ => snapshot }
    }(using mayJoinRV)
  */

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    // Not and WeakNot only differ in their typing discipline
    case Not(atom) => evalAtomOpen(negateAtom(atom))
    case WeakNot(atom) => evalAtomOpen(negateAtom(atom))
    case _ => super.evalAtomOpen(at)