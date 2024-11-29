package inca.ir.analysis

import inca.ir
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, CSupplementaryTable, FixIn, FixOut}
import inca.ir.analysis.base.logger.PrintLogger
import inca.ir.analysis.base.values.*
import inca.ir.extension.arithmetic.analysis as irarith
import inca.ir.extension.string.analysis as irstr
import inca.ir.extension.data.analysis as irdata
import sturdy.data.MayJoin.{NoJoin, WithJoin}
import sturdy.effect.except.{Except, JoinedExcept}
import sturdy.effect.failure.{CollectedFailures, Failure}
import sturdy.effect.store.AStoreThreaded
import sturdy.effect.EffectStack
import sturdy.fix
import sturdy.fix.StackConfig.{StackedCfgNodes, StackedStates}
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.booleans.{BooleanBranching, BooleanOps, ConcreteBooleanBranching, ConcreteBooleanOps}
import sturdy.values.ordering.EqOps
import sturdy.values.references.{AllocationSiteAddr, given_Finite_AllocationSiteAddr}
import sturdy.values.*

// Implicits
import sturdy.data.given
import sturdy.values.given
import inca.ir.analysis.base.effect.IRFailure
import inca.ir.analysis.base.effect.IRException
import inca.ir.analysis.base.interpreter.FiniteFixIn
import inca.ir.analysis.base.values.JoinCRV
import sturdy.data.MakeJoined
import sturdy.values.exceptions.PowersetExceptional


given CCombineFixOut[W <: Widening]: Combine[FixOut[Value, CRelationValue[Value]], W] with

  override def apply(out1: FixOut[Value, CRelationValue[Value]], out2: FixOut[Value, CRelationValue[Value]]): MaybeChanged[FixOut[Value, CRelationValue[Value]]] =
    (out1, out2) match
      case (FixOut.Term(rv1), FixOut.Term(rv2)) => assert(rv1 == rv2); MaybeChanged(FixOut.Term(rv1), out1)
      case (FixOut.Atom(), FixOut.Atom()) => Unchanged(FixOut.Atom())
      case (FixOut.ExitCall(rv1), FixOut.ExitCall(rv2)) => MaybeChanged(FixOut.ExitCall(rv1.join(rv2)), out1)
      case (FixOut.Body(rv1), FixOut.Body(rv2)) => MaybeChanged(FixOut.Body(rv1.join(rv2)), out1)
      case (FixOut.Relation(rv1), FixOut.Relation(rv2)) => MaybeChanged(FixOut.Relation(rv1.join(rv2)), out1)
      case _ => throw new IllegalArgumentException(s"Cannot combine outputs of different kind, $out1 and $out2")


class IRConcreteInterpreter(val enableLogging: Boolean = false)
  extends BaseGenericInterpreter[Value, Boolean, CRelationValue[Value], Powerset[BaseIRException], NoJoin]
  with irarith.interpreter.ConcreteInterpreter
  with irstr.interpreter.ConcreteInterpreter
  with irdata.interpreter.ConcreteInterpreter:

  type CRV = CRelationValue[Value]

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures

  given Failure = failure

  override val boolOps: BooleanOps[Boolean] = ConcreteBooleanOps

  // TODO: Which kind of ExcV should we use here?
  override lazy val except: Except[BaseIRException, Powerset[BaseIRException], WithJoin] = new JoinedExcept(using PowersetExceptional[BaseIRException])

  given BooleanOps[Boolean] = boolOps

  override val branchOps: BooleanBranching[Boolean, CRV] = ConcreteBooleanBranching


  override lazy val eqOps: EqOps[Value, Boolean] = new EqOps[Value, Boolean] {
    def equ(v1: Value, v2: Value): Boolean = v1 == v2
    def neq(v1: Value, v2: Value): Boolean = v1 != v2
  }
  
  override val joinV: NoJoin[Value] = implicitly
  override val joinRV: Join[CRV] = implicitly
  
  // Only correct for concrete Datalog interpreter
  given Widen[CRV] with {
    override def apply(v1: CRV, v2: CRV): MaybeChanged[CRV] = joinRV(v1, v2)
  }
  
  override val joinUnit: NoJoin[Unit] = implicitly

  override val supplementaryTable: CSupplementaryTable = new CSupplementaryTable
  override val idb: AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, CRV] = AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, CRV](Map())

  override def resetIDB(): Unit = idb.setState(Map())

  given EqOps[Value, Boolean] = eqOps

  override val relationOps: RelationOps[Value, Boolean, CRV] = new CRelationValueOps[Value]


  fix.Fixpoint.DEBUG = false

  override val fixpoint: EffectStack ?=> fix.Fixpoint[FixIn, FixOut[Value, CRV]] =
    val fixPt =
      fix.notContextSensitive[FixIn, FixOut[Value, CRV], fix.Combinator[FixIn, FixOut[Value, CRV]]](
        fix.filter({
          case _: FixIn.EnterRelation => true
          case _ => false // important, filter everything out we don't need
        }, fix.iter.innermost[FixIn, FixOut[Value, CRV], Unit](StackedStates()))
          //fix.iter.innermost[FixIn, FixOut[Value, CRV], Unit](StackedCfgNodes()))
          //fix.iter.outermost[FixIn, FixOut[Value, CRV], Unit, Unit, Unit, Unit](StackedStates(readPriorOutput = true)))
        )

    if (enableLogging)
      fix.log(new PrintLogger, fixPt).fixpoint
    else
      fixPt.fixpoint


    /*val fixpt = new ContextualFixpoint[FixIn, FixOut[Value, CRV]] {
      override type Ctx = CallString

      // 1-context-sensitive should be enough.
      override protected def context: Sensitivity[FixIn, Ctx] = new Sensitivity[FixIn, Ctx] {
        def emptyContext: Ctx = null.asInstanceOf[Ctx]
        def switchCall(dom: FixIn): Boolean = dom match
          case FixIn.EnterCall(r, params, args, neg) => true
          case _ => false
        override def apply(dom: FixIn): Ctx = dom match
          case FixIn.EnterCall(r, params, args, neg) =>
            ContextCallString(Seq((r.name, args, neg)))
          case _ => ContextCallString(Seq())
      }

      // TODO: Not sure what this does
      override protected def contextFree: Combinator[FixIn, FixOut[Value, CRV]] => Combinator[FixIn, FixOut[Value, CRV]] = f => f

      override protected def contextSensitive: Contextual[Ctx, FixIn, FixOut[Value, CRV]] ?=> Combinator[FixIn, FixOut[Value, CRV]] =
        fix.iter.innermost(StackedStates())
    }

    fixpt.addContextSensitiveLogger(contextual ?=> new PrintLogger(contextual))
    fixpt*/



