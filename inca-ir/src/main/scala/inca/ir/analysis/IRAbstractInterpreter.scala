package inca.ir.analysis

import inca.ir.extension.arithmetic.analysis as arith
import inca.ir.Name
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.values.{BaseJoinV, FiniteV, JoinVBool, RelationValue, RelationValueOps, Top, VBool, VBoolOps, Value}
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, FixIn, FixOut, SupplementaryTable}
import inca.ir.analysis.base.ordering.BaseEqOps
import sturdy.data.WithJoin
import sturdy.values.{Changed, Finite, Join, MaybeChanged, Widen, Widening, finitely}
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.effect.failure.{CollectedFailures, Failure}
import sturdy.effect.store.{AStoreThreaded, Store}
import sturdy.fix
import sturdy.data.finiteUnit
import sturdy.fix.{Combinator, ContextInsensitiveFixpoint, Contextual, Fixpoint, Logger}
import sturdy.fix.StackConfig.StackedStates
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.booleans.BooleanOps
import sturdy.values.references.AllocationSiteAddr
import sturdy.values.references.given_Finite_AllocationSiteAddr

// Implicits
import inca.ir.analysis.base.effect.IRFailure
import inca.ir.analysis.base.values.{ FiniteRV, JoinRV }
import inca.ir.analysis.base.interpreter.{ FiniteFixIn, FiniteFixOut }
import inca.ir.analysis.base.interpreter.CombineFixOut


class IRJoinV extends Join[Value]
  with BaseJoinV
  with arith.values.JoinV:

  override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
    val joined = join(v1, v2)
    if v1 == joined then
      Unchanged(joined)
    else
      Changed(joined)

class IREqOps(using boolOps: BooleanOps[VBool]) extends BaseEqOps
  with arith.ordering.EqOps


class IRAbstractInterpreter extends BaseGenericInterpreter[Value, VBool, RelationValue[Value], WithJoin]
  with arith.interpreter.ConstantAbstractInterpreter:

  type RV = RelationValue[Value]

  override val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures
  override val except = ???

  override val boolOps: BooleanOps[VBool] = new VBoolOps(using failure)
  override val boolTop: VBool = VBool.Top

  override val eqOps: BaseEqOps = new IREqOps(using boolOps)

  given IRJoinV: Join[Value]
  override val joinV: WithJoin[Value] = implicitly //new IRJoinV
  
  private val finiteV: Finite[Value] = new FiniteV
  private val widenV: Widen[Value] = finitely(using joinV, finiteV)
  override val top: Value = Top

  override val joinRV: Join[RV] = new JoinRV(using joinV)
  private val finiteRV: Finite[RV] = new FiniteRV
  private val widenRV: Widen[RV] = finitely(using joinRV, finiteRV)

  override val supplementaryEnv: SupplementaryTable = new SupplementaryTable(using joinRV, widenRV, failure)
  override val IDB: Store[AllocationSiteAddr, RV, WithJoin] = AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, RV](Map())(using joinRV, widenRV, implicitly)
  override val effects: EffectStack = EffectStack(supplementaryEnv, failure, IDB)

  override val relationOps: RelationOps[Value, VBool, RV] = new RelationValueOps[Value, VBool](using effects, joinV, boolOps, eqOps, failure) {}

  // TODO: Use context sensitive fixpoint combinator
  override val fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[Value, RV]] =
    val fixpt = new ContextInsensitiveFixpoint[FixIn, FixOut[Value, RV]] {
      override protected def contextInsensitive: Contextual[Unit, FixIn, FixOut[Value, RV]] ?=> Combinator[FixIn, FixOut[Value, RV]] =
        given Join[RV] = joinRV
        //given Finite[RV] = finiteRV
        //given Widen[RV] = widenRV
        //given Join[Value] = joinV
        given Finite[Value] = finiteV
        given Widen[Value] = widenV
        given Join[VBool] = new JoinVBool
        fix.filter(_.isLoop, fix.iter.innermost(StackedStates()))
    }

    fixpt.addContextFreeLogger(new Logger[FixIn, FixOut[Value, RV]] {
      override def enter(dom: FixIn): Unit = dom match
        case _ => // println(s"Enter: $dom")

      override def exit(dom: FixIn, codom: TrySturdy[FixOut[Value, RV]]): Unit = (dom, codom.getOrThrow) match
        case (_, FixOut.Term(_)) =>
        case _ =>
          println(s"Exit:\n$dom\nResult: $codom")
        //case _ => // nothing
    })

    fixpt



  /*val observedConfig = config.withObservers(Seq())
  override val fixpoint: fix.ContextualFixpoint[FixIn, FixOut[RV]] = new fix.ContextualFixpoint {
    override type Ctx = observedConfig.ctx.Ctx
    val (contextPreparation, sensitivity) = observedConfig.ctx.make[RV]
    import observedConfig.ctx.finiteCtx
    override protected def contextFree = phi =>
      fix.log(controlEventLogger(Instance.this, effectStack, except), contextPreparation(phi))
    override protected def context: Sensitivity[FixIn, Ctx] = sensitivity
    override protected def contextSensitive = observedConfig.fix.get
  }*/
