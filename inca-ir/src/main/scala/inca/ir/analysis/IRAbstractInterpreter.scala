package inca.ir.analysis

import inca.ir.extension.arithmetic.analysis as arith
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.values.{BaseJoinV, RelationValue, RelationValueOps, VBool, VBoolOps, Value}
import inca.ir.analysis.base.effect.Failure as IRFailure
import inca.ir.analysis.base.interpreter.{BaseAbstractInterpreter, FixIn, FixOut, SupplementaryTable}
import inca.ir.analysis.base.ordering.BaseEqOps
import sturdy.data.WithJoin
import sturdy.values.finitely
import sturdy.effect.EffectStack
import sturdy.effect.failure.{CollectedFailures, Failure}
import sturdy.effect.store.{AStoreThreaded, Store}
import sturdy.fix
import sturdy.data.finiteUnit
import sturdy.fix.{Combinator, ContextInsensitiveFixpoint, Contextual, Fixpoint}
import sturdy.fix.StackConfig.StackedStates
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.{Changed, Join, MaybeChanged}
import sturdy.values.booleans.BooleanOps
import sturdy.values.references.AllocationSiteAddr
import sturdy.values.references.given_Finite_AllocationSiteAddr

// Implicits
import inca.ir.analysis.base.effect.IRFailure
import inca.ir.analysis.base.values.{ FiniteRV, JoinRV }
import inca.ir.analysis.base.interpreter.finiteFixIn
import inca.ir.analysis.base.interpreter.CombineFixOut


class IRJoinV extends Join[Value]
  with BaseJoinV
  with arith.values.JoinV:

  override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
    if v1 == v2 then
      Unchanged(v1)
    else
      Changed(join(v1, v2))


class IREqOps(using boolOps: VBoolOps) extends BaseEqOps
  with arith.ordering.EqOps


class IRAbstractInterpreter extends BaseAbstractInterpreter
  with arith.interpreter.AbstractInterpreter:

  override val fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[RelationValue]] = new ContextInsensitiveFixpoint[FixIn, FixOut[RelationValue]] {
    override protected def contextInsensitive: Contextual[Unit, FixIn, FixOut[RelationValue]] ?=> Combinator[FixIn, FixOut[RelationValue]] =
      fix.iter.innermost(StackedStates())
  }

  override val failure: CollectedFailures[effect.Failure] = new CollectedFailures
  given Failure = failure

  override val boolOps: VBoolOps = new VBoolOps
  given VBoolOps = boolOps

  override val eqOps: BaseEqOps = new IREqOps
  given BaseEqOps = eqOps

  def joinRV: Join[RelationValue] = implicitly
  override val joinV: Join[Value] = new IRJoinV
  //given Join[Value] = joinV

  override val relationOps: RelationValueOps = new RelationValueOps

  override val supplementaryTable: SupplementaryTable = new SupplementaryTable
  override val IDB: Store[AllocationSiteAddr, RelationValue, WithJoin] = AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, RelationValue](Map())
  override val effects: EffectStack = EffectStack(supplementaryTable, failure, IDB)
