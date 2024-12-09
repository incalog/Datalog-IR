package inca.ir.analysis

import inca.ir.extension.arithmetic.analysis as arith
import inca.ir.Name
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{ARelationValue, ARelationValueOps, BaseJoinV, FiniteV, Top, Value}
import inca.ir.analysis.base.interpreter.{ASupplementaryTable, BaseGenericInterpreter, FixIn, FixOut}
import inca.ir.analysis.base.ordering.BaseEqOps
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
import sturdy.values.booleans.{BooleanBranching, BooleanOps, ToppedBooleanBranching}
import sturdy.values.ordering.EqOps
import sturdy.values.references.AllocationSiteAddr
import sturdy.values.references.given_Finite_AllocationSiteAddr

// Implicits
import sturdy.data.given 
import inca.ir.analysis.base.effect.IRFailure
import inca.ir.analysis.base.values.{FiniteARelationValue, JoinRV}
//import inca.ir.analysis.base.interpreter.FiniteFixIn
import sturdy.values.booleans.ConcreteBooleanBranching
import sturdy.values.exceptions.PowersetExceptional
import sturdy.values.given
import inca.ir.analysis.base.effect.IRException

private class IRJoinV extends Join[Value]
  with BaseJoinV
  with arith.interpreter.ConstantJoinV:
  // TODO: inherit from rest

  override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
    val joined = join(v1, v2)
    if v1 == joined then
      Unchanged(joined)
    else
      Changed(joined)

private class IREqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps
  with arith.interpreter.ConstantEqOps
  // TODO: inherit from rest

class IRConstantAbstractInterpreter
  extends BaseGenericInterpreter[Value, Topped[Boolean], ARelationValue[Value], Powerset[BaseIRException], WithJoin]
  with arith.interpreter.ConstantAbstractInterpreter:
  // TODO: inherit from rest

  type RV = ARelationValue[Value]

  override lazy val except: Except[BaseIRException, Powerset[BaseIRException], WithJoin] = new JoinedExcept(using PowersetExceptional[BaseIRException])

  override val branchOps: BooleanBranching[Topped[Boolean], RV] = new ToppedBooleanBranching[Boolean, RV]

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures

  override val boolOps: BooleanOps[Topped[Boolean]] = implicitly

  given BooleanOps[Topped[Boolean]] = boolOps

  override lazy val eqOps: BaseEqOps = new IREqOps

  given Join[Value] = new IRJoinV

  given Join[RV] = new JoinRV

  given Finite[RV] = new FiniteARelationValue
  
  override val joinV: WithJoin[Value] = implicitly
  override val joinRV: Join[RV] = ???
  override val joinUnit: WithJoin[Unit] = implicitly

  override lazy val supplementaryTable: SupplementaryTable[ARelationValue[Value]] = ???
  override lazy val idb: AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, RV] = AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, RV](Map())

  given EqOps[Value, Topped[Boolean]] = eqOps

  override val relationOps: RelationOps[Value, Topped[Boolean], RV] = new ARelationValueOps[Value, Topped[Boolean]]

  override def resetIDB(): Unit = idb.setState(Map())
  
  override val fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[Value, RV]] = ???