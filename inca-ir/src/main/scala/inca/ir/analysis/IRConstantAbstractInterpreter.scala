package inca.ir.analysis

import inca.ir.extension.arithmetic.analysis as arith
import inca.ir.Name
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{ARelationValue, ARelationValueOps, BaseJoinV, FiniteV, JoinVBool, Top, VBool, VBoolOps, Value}
import inca.ir.analysis.base.interpreter.{ASupplementaryTable, BaseGenericInterpreter, FixIn, FixOut}
import inca.ir.analysis.base.ordering.BaseEqOps
import sturdy.data.WithJoin
import sturdy.values.{Changed, Combine, Finite, Join, MaybeChanged, Widen, Widening, finitely}
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
import inca.ir.analysis.base.values.{FiniteARelationValue, JoinRV}
import inca.ir.analysis.base.interpreter.{FiniteFixIn, FiniteFixOut}

private class IRJoinV extends Join[Value]
  with BaseJoinV
  with arith.interpreter.ConstantJoinV:

  override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
    val joined = join(v1, v2)
    if v1 == joined then
      Unchanged(joined)
    else
      Changed(joined)

private class IREqOps(using boolOps: BooleanOps[VBool]) extends BaseEqOps
  with arith.interpreter.ConstantEqOps


// TODO: What widen value should be here? 
given CombineFixOut[V, RV, VW <: Widening, RW <: Widening](using combineV: Combine[V, VW], combineRV: Combine[RV, RW]): Combine[FixOut[V, RV], Widening.No] with
  override def apply(out1: FixOut[V, RV], out2: FixOut[V, RV]): MaybeChanged[FixOut[V, RV]] = (out1, out2) match
    case (FixOut.Term(vs1), FixOut.Term(vs2)) =>
      // We use a cartesian product here because of Datalog set semantics
      val v = for (v1 <- vs1; v2 <- vs2) yield combineV(v1, v2)
      if (v.exists(_.hasChanged)) {
        Changed(FixOut.Term(v.map(_.get)))
      } else {
        Unchanged(FixOut.Term(v.map(_.get)))
      }
    case (FixOut.Atom(), FixOut.Atom()) => Unchanged(FixOut.Atom())
    case (FixOut.ExitCall(rv1), FixOut.ExitCall(rv2)) => combineRV(rv1, rv2).map(FixOut.ExitCall.apply)
    case (FixOut.Body(rv1), FixOut.Body(rv2)) => combineRV(rv1, rv2).map(FixOut.Body.apply)
    case (FixOut.Relation(rv1), FixOut.Relation(rv2)) => combineRV(rv1, rv2).map(FixOut.Relation.apply)
    case (FixOut.ExtensionalRelation(rv1), FixOut.ExtensionalRelation(rv2)) => combineRV(rv1, rv2).map(FixOut.ExtensionalRelation.apply)
    case (FixOut.Module(idb1), FixOut.Module(idb2)) =>
      val allKeys = idb1.keys ++ idb2.keys
      val res = for (k <- allKeys) yield
        (idb1.get(k), idb2.get(k)) match
          case (Some(rv1), Some(rv2)) => k -> combineRV(rv1, rv2).get
          case (Some(rv1), _) => k -> rv1
          case (_, Some(rv2)) => k -> rv2
          case _ => throw IllegalStateException(s"IDB key not found: $k")
      val idb = res.toMap
      if (idb != idb1) {
        Changed(FixOut.Module(idb))
      } else {
        Unchanged(FixOut.Module(idb))
      }

    case _ => throw new IllegalArgumentException(s"Cannot combine outputs of different kind, $out1 and $out2")

class IRConstantAbstractInterpreter extends BaseGenericInterpreter[Value, VBool, ARelationValue[Value], WithJoin]
  with arith.interpreter.ConstantAbstractInterpreter[WithJoin]:

  type RV = ARelationValue[Value]

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
  //override val boolTop: VBool = VBool.Top

  given BooleanOps[VBool] = boolOps

  override val eqOps: BaseEqOps = new IREqOps

  given Join[Value] = new IRJoinV

  given Join[RV] = new JoinRV

  given Finite[RV] = new FiniteARelationValue

  given Join[VBool] = new JoinVBool

  override val joinV: WithJoin[Value] = implicitly
  override val joinRV: WithJoin[RV] = implicitly
  override val joinUnit: WithJoin[Unit] = implicitly

  override lazy val supplementaryTable: ASupplementaryTable = new ASupplementaryTable
  override lazy val IDB: Store[AllocationSiteAddr, RV, WithJoin] = AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, RV](Map())
  // lazy is important because of cyclic implicits
  //override lazy val effects: EffectStack = EffectStack(supplementaryTable, failure, IDB)

  //override val top: Value = Top

  given EqOps[Value, VBool] = eqOps

  override val relationOps: RelationOps[Value, VBool, RV] = new ARelationValueOps[Value, VBool] {}

  // TODO: Use context sensitive fixpoint combinator
  override val fixpoint: EffectStack ?=> Fixpoint[FixIn, FixOut[Value, RV]] =
    val fixpt = new ContextInsensitiveFixpoint[FixIn, FixOut[Value, RV]] {
      // TODO: Would should contextual be?
      override protected def contextInsensitive: Contextual[Unit, FixIn, FixOut[Value, RV]] ?=> Combinator[FixIn, FixOut[Value, RV]] =
        //fix.filter(_.isLoop, fix.iter.innermost(StackedStates()))
        fix.iter.innermost(StackedStates())
    }

    var indent: Int = -1

    def printlnWithIndent(msg: String): Unit =
      val indentS = "    ".repeat(indent)
      println(s"$indentS$msg")

    fixpt.addContextFreeLogger(new Logger[FixIn, FixOut[Value, RV]] {
      override def enter(dom: FixIn): Unit =
        dom match
          case FixIn.Term(term) =>
          case FixIn.Atom(atom) => printlnWithIndent(s"Enter atom: $atom")
          case FixIn.EnterCall(_, _, _, _) => printlnWithIndent(s"Enter call: $dom")
          case FixIn.Body(body) => printlnWithIndent(s"Enter body: ${body.hashCode()}")
          case FixIn.Relation(rel) => printlnWithIndent(s"Enter relation: ${rel.name}")
          case FixIn.ExtensionalRelation(rel) =>
          case FixIn.Module(mod) =>
        indent += 1

      override def exit(dom: FixIn, codom: TrySturdy[FixOut[Value, RV]]): Unit =
        indent -= 1
        (dom, codom) match // getOrThrow
          //case (_, FixOut.Term(_)) =>

          case (FixIn.Atom(atom), _) => printlnWithIndent(s"Exit atom: $atom\tResult: $codom")
          case (FixIn.EnterCall(_, _, _, _), _) =>
            printlnWithIndent(s"Exit call: $dom\tResult: $codom")
          case (FixIn.Body(body), _) => printlnWithIndent(s"Exit body: ${body.hashCode()}\tResult: $codom")
          case (FixIn.Relation(rel), _) => printlnWithIndent(s"Exit relation: ${rel.name}\tResult: $codom")
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
