package inca.ir.analysis

import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{BaseJoinV, ConstantRelation, ConstantRelationOps, FiniteV, Top, TypeValue, Value}
import inca.ir.analysis.base.interpreter.{ASupplementaryTable, BaseGenericInterpreter, FixIn, FixOut, SupColumn}
import inca.ir.analysis.base.logger.{BaseAnalysisAnnotator, PrintLogger}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.extension.arithmetic.analysis as irarith
import inca.ir.extension.data.analysis as irdata
import inca.ir.extension.string.analysis as irstr
import sturdy.data.WithJoin
import sturdy.values.{Changed, Combine, Finite, Join, MaybeChanged, Powerset, Topped, Widen, Widening, finitely}
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.effect.failure.{CollectedFailures, Failure}
import sturdy.effect.store.{AStoreThreaded, Store}
import sturdy.fix
import sturdy.data.finiteUnit
import sturdy.effect.except.{Except, JoinedExcept}
import sturdy.fix.{Combinator, ContextInsensitiveFixpoint, Contextual, Fixpoint, Logger}
import sturdy.fix.StackConfig.StackedStates
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.booleans.{BooleanBranching, BooleanOps, ConcreteBooleanOps, ToppedBooleanBranching, ToppedBooleanOps}
import sturdy.values.ordering.EqOps
import sturdy.values.references.AllocationSiteAddr
import sturdy.values.references.given_Finite_AllocationSiteAddr

// Implicits
import sturdy.data.given 
import inca.ir.analysis.base.effect.IRFailure
import inca.ir.analysis.base.values.JoinRV
import inca.ir.analysis.base.interpreter.FiniteFixIn
import sturdy.values.booleans.ConcreteBooleanBranching
import sturdy.values.exceptions.PowersetExceptional
import sturdy.values.given
import inca.ir.analysis.base.effect.IRException
import inca.ir.analysis.base.interpreter.CCombineFixOut

/*private class IRMeetV extends BaseMeetV
  with irarith.interpreter.ConstantMeetV
  with irstr.interpreter.ConstantMeetV*/

private class IRJoinV extends Join[Value] with BaseJoinV
  with irarith.interpreter.ConstantJoinV
  with irstr.interpreter.ConstantJoinV:
  // TODO: inherit from rest

  override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
    val joined = join(v1, v2)
    if v1 == joined then
      Unchanged(joined)
    else
      Changed(joined)

private class IREqOps extends BaseEqOps
  with irarith.interpreter.ConstantEqOps
  with irstr.interpreter.ConstantEqOps
  // TODO: inherit from rest

class IRConstantAbstractInterpreter(val enableLogging: Boolean = false)
  extends BaseGenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]
  with irarith.interpreter.ConstantAbstractInterpreter
  with irstr.interpreter.ConstantAbstractInterpreter:
  // TODO: inherit from rest

  type RV = ConstantRelation

  override lazy val except: Except[BaseIRException, Powerset[BaseIRException], WithJoin] = new JoinedExcept(using PowersetExceptional[BaseIRException])

  override val branchOps: BooleanBranching[Topped[Boolean], RV] = new ToppedBooleanBranching[Boolean, RV]

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures

  override val boolOps: BooleanOps[Topped[Boolean]] = new ToppedBooleanOps

  given BooleanOps[Topped[Boolean]] = boolOps

  override lazy val eqOps: BaseEqOps = new IREqOps

  given Join[Value] = new IRJoinV

  given Join[RV] = new JoinRV

  override val joinV: WithJoin[Value] = implicitly
  override val joinRV: Join[RV] = implicitly
  override val joinUnit: WithJoin[Unit] = implicitly

  // I don't think we need to widen tables for a constant analysis
  given Widen[RV] with {
    override def apply(v1: RV, v2: RV): MaybeChanged[RV] = joinRV(v1, v2)
  }
  
  override lazy val supplementaryTable: SupplementaryTable[ConstantRelation] = new ASupplementaryTable[RV]() {
    override def initialTable: RV = ConstantRelation(Seq(), Seq(), Topped.Actual(false))
  }
  override lazy val idb: AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, RV] = AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, RV](Map())

  given EqOps[Value, Topped[Boolean]] = eqOps

  //given BaseMeetV = IRMeetV()

  override val relationOps: RelationOps[Value, Topped[Boolean], RV] = new ConstantRelationOps

  override def resetIDB(): Unit = idb.setState(Map())

  class AnalysisAnnotator
    extends BaseAnalysisAnnotator[Value, RV, Value]
      with irarith.logger.AnalysisAnnotator[Value, RV, Value]
      with irdata.logger.AnalysisAnnotator[Value, RV, Value]
      with irstr.logger.AnalysisAnnotator[Value, RV, Value]:

    override def extractTermValue(supName: SupColumn): Value =
      val supTable = supplementaryTable.getTable
      val termTRV = relationOps.project(supTable, Seq(supName))
      assert(termTRV.rows.size == 1)
      termTRV.rows.head

  val analysisAnnotator: AnalysisAnnotator = new AnalysisAnnotator

  fix.Fixpoint.DEBUG = false

  override val fixpoint: EffectStack ?=> fix.Fixpoint[FixIn, FixOut[Value, RV]] =
    val fixPt =
      fix.notContextSensitive[FixIn, FixOut[Value, RV], fix.Combinator[FixIn, FixOut[Value, RV]]](
        fix.filter({
          case _: FixIn.EnterRelation => true
          case _ => false // important, filter everything out we don't need
        }, fix.iter.innermost[FixIn, FixOut[Value, RV], Unit](StackedStates()))
      )

    val analysisFixPt = fix.log(analysisAnnotator, fixPt)

    if (enableLogging)
      fix.log(new PrintLogger, analysisFixPt).fixpoint
    else
      analysisFixPt.fixpoint