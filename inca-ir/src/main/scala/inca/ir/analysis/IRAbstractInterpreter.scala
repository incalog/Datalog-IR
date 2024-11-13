package inca.ir.analysis

import inca.ir.extension.arithmetic.analysis as arith
import inca.ir.Name
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
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
import sturdy.effect.except.JoinedExcept
import sturdy.fix.{Combinator, ContextInsensitiveFixpoint, Contextual, Fixpoint, Logger}
import sturdy.fix.StackConfig.StackedStates
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps
import sturdy.values.references.AllocationSiteAddr
import sturdy.values.references.given_Finite_AllocationSiteAddr
import sturdy.values.exceptions.Exceptional

// Implicits
import sturdy.data.MakeJoined
import inca.ir.analysis.base.effect.IRFailure
import inca.ir.analysis.base.values.{FiniteRV, JoinRV}
import inca.ir.analysis.base.interpreter.{FiniteFixIn, FiniteFixOut}
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
  with arith.interpreter.ConstantAbstractInterpreter[WithJoin]:

  type RV = RelationValue[Value]

  given Exceptional[BaseIRException, BaseIRException, WithJoin] = new Exceptional[BaseIRException, BaseIRException, WithJoin] {
    override def exception(exc: BaseIRException): BaseIRException = exc

    override def handle[A](e: BaseIRException)(f: BaseIRException => A): WithJoin[A] ?=> A = f(e)
  }

  /*given Finite[BaseIRException] = new Finite[BaseIRException] {}

  given Join[BaseIRException] with {
    override def apply(v1: BaseIRException, v2: BaseIRException): MaybeChanged[BaseIRException] =
      ???
  }*/

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures
  //override lazy val except = new JoinedExcept[BaseIRException, BaseIRException]

  given Failure = failure

  override val boolOps: BooleanOps[VBool] = new VBoolOps
  override val boolTop: VBool = VBool.Top

  given BooleanOps[VBool] = boolOps

  override val eqOps: BaseEqOps = new IREqOps

  given Join[Value] = new IRJoinV

  given Join[RV] = new JoinRV

  given Finite[RV] = new FiniteRV

  given Join[VBool] = new JoinVBool

  override val joinV: WithJoin[Value] = implicitly
  override val joinRV: WithJoin[RV] = implicitly
  override val joinUnit: WithJoin[Unit] = implicitly

  override lazy val supplementaryEnv: SupplementaryTable = new SupplementaryTable
  override lazy val IDB: Store[AllocationSiteAddr, RV, WithJoin] = AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, RV](Map())
  // lazy is important because of cyclic implicits
  override lazy val effects: EffectStack = EffectStack(supplementaryEnv, failure, IDB)

  override val top: Value = Top

  given EqOps[Value, VBool] = eqOps

  override val relationOps: RelationOps[Value, VBool, RV] = new RelationValueOps[Value, VBool] {}

  // TODO: Use context sensitive fixpoint combinator
  override val fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[Value, RV]] =
    val fixpt = new ContextInsensitiveFixpoint[FixIn, FixOut[Value, RV]] {
      override protected def contextInsensitive: Contextual[Unit, FixIn, FixOut[Value, RV]] ?=> Combinator[FixIn, FixOut[Value, RV]] =
        //fix.filter(_.isLoop, fix.iter.innermost(StackedStates()))
        fix.iter.innermost(StackedStates())
    }

    fixpt.addContextFreeLogger(new Logger[FixIn, FixOut[Value, RV]] {
      override def enter(dom: FixIn): Unit = dom match
        case _ => dom match
          case FixIn.Term(term) =>
          case FixIn.Atom(atom) => println(s"Enter atom: $atom")
          case FixIn.EnterCall(_, _, _, _) => println(s"Enter call: $dom")
          case FixIn.Body(body) => println(s"Enter body: ${body.hashCode()}")
          case FixIn.Relation(rel) => println(s"Enter relation: ${rel.name}")
          case FixIn.ExtensionalRelation(rel) =>
          case FixIn.Module(mod) =>

      override def exit(dom: FixIn, codom: TrySturdy[FixOut[Value, RV]]): Unit =
        (dom, codom) match // getOrThrow
          //case (_, FixOut.Term(_)) =>

          case (FixIn.Atom(atom), _) => println(s"Exit atom: $atom\tResult: $codom")
          case (FixIn.EnterCall(_, _, _, _), _) => println(s"Exit call: $dom\tResult: $codom")
          case (FixIn.Body(body), _) => println(s"Exit body: ${body.hashCode()}\tResult: $codom")
          case (FixIn.Relation(rel), _) => println(s"Exit relation: ${rel.name}\tResult: $codom")
          case (FixIn.Module(mod), _) =>
            println(s"Module: ${codom.getOrThrow}")
          //println(s"Exit:\n$dom\nResult: $codom")
          case _ => // nothing
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
