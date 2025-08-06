package inca.ir.extension.disjunction.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import sturdy.data.{MayJoin, mapJoin, MakeJoined}

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:

  override def evalAtomOpen(at: Atom)(using Fixed): Unit = at match
    case Disjunction(Seq()) => // nothing
    case Disjunction(alternatives) =>
      updateSupplementaryChecked { supBefore =>
        val colsBefore = relationOps.columns(supBefore)
        val boundAfterDisjunction = (at.commonVars.map(_.name.name) ++ colsBefore).toSeq

        mapJoin(alternatives, { alt =>
          scopedSupplementary { _ =>
            evalAtomGroup(alt.body.atoms)
            val sup = supplementaryTable.getTable
            relationOps.project(sup, boundAfterDisjunction)
          }
        })
      }
    case _ => super.evalAtomOpen(at)
