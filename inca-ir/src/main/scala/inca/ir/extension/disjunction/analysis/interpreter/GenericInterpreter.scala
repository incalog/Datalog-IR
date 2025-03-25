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
      val before = snapshotSupplementary()
      val colsBefore = relationOps.columns(before)

      // all variables that are in scope after the disjunction
      val allVars = alternatives.map(_.body.vars.map(_.name.name))
      val boundAfterDisjunction = allVars.foldLeft[Seq[String]](allVars.flatten) { (acc, altVars) =>
        acc.intersect(altVars)
      } ++ colsBefore

      val joinedRes = mapJoin(alternatives, { alt =>
        scopedSupplementary {
          evalAtoms(alt.body.atoms)
          val sup = supplementaryTable.getTable
          relationOps.project(sup, boundAfterDisjunction)
        }
      })
      updateSupplementaryChecked(_ => joinedRes)
    case _ => super.evalAtomOpen(at)
