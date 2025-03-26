package inca.ir.extension.bool.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.analysis.base.effect.BaseIRFailure
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.bool.*
import inca.ir.visitors.IRVisitor
import sturdy.data.MayJoin
import sturdy.values.booleans.BooleanOps

case object InvalidBooleanOp extends BaseIRFailure

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]
  with IRVisitor: // we use the visitor to negate atoms

  val booleanOps: BooleanOps[V]

  override protected def canDetermineValue(term: ir.Term): Boolean = term match
    case AtomAsBool(a) => true
    case BoolAnd(t1, t2) => canDetermineValue(t1) && canDetermineValue(t2)
    case BoolOr(t1, t2) => canDetermineValue(t1) && canDetermineValue(t2)
    case BoolNot(t) => canDetermineValue(t)
    case BoolTrue => true
    case BoolFalse => true
    case _ => super.canDetermineValue(term)

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case AtomAsBool(a) =>
      val resName = gensym.fresh("result")

      val sup = snapshotSupplementary()
      val colsBefore = relationOps.columns(sup)
      val posBranchVars = a.vars.map(_.name.name)
      val negBranchVars = negateAtom(a).vars.map(_.name.name)
      val commonCols = colsBefore ++ posBranchVars.intersect(negBranchVars) :+ resName

      val joinedSup = effects.joinComputations {
        val res = scopedSupplementary { _ => 
          evalAtomGroup(Seq(a))
          relationOps.map(supplementaryTable.getTable, resName) { _ => booleanOps.boolLit(true) }
        }
        relationOps.project(res, commonCols)
      } {
        val res = scopedSupplementary { _ =>
          evalAtomGroup(Seq(negateAtom(a)))
          relationOps.map(supplementaryTable.getTable, resName) { _ => booleanOps.boolLit(false) }
        }
        relationOps.project(res, commonCols)
      }

      updateSupplementaryChecked(_ => joinedSup)
      resName
    case BoolAnd(t1, t2) => binaryOp(evalTerm(t1), evalTerm(t2))(booleanOps.and)
    case BoolOr(t1, t2) => binaryOp(evalTerm(t1), evalTerm(t2))(booleanOps.or)
    case BoolNot(t) => unaryOp(evalTerm(t))(booleanOps.not)
    case BoolTrue => termResult(booleanOps.boolLit(true))
    case BoolFalse => termResult(booleanOps.boolLit(false))
    case _ => super.evalTermOpen(term)

