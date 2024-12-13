package inca.ir.analysis

import inca.ir
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, CSupplementaryTable, FixIn, FixOut, given}
import inca.ir.analysis.base.logger.PrintLogger
import inca.ir.analysis.base.values.*
import inca.ir.extension.arithmetic.analysis as irarith
import inca.ir.extension.string.analysis as irstr
import inca.ir.extension.data.analysis as irdata
import sturdy.data.MayJoin.{NoJoin, WithJoin}
import sturdy.effect.except.{Except, JoinedExcept}
import sturdy.effect.failure.CollectedFailures
import sturdy.effect.store.AStoreThreaded
import sturdy.effect.EffectStack
import sturdy.fix
import sturdy.fix.StackConfig.{StackedCfgNodes, StackedStates}
import sturdy.fix.context.Parameters
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
import sturdy.values.exceptions.PowersetExceptional
import sturdy.fix.context.FiniteParameters
import inca.ir.analysis.base.values.FiniteV


class IRConcreteInterpreter(val enableLogging: Boolean = false)
  extends BaseGenericInterpreter[Value, Boolean, ConcreteRelation[Value], Powerset[BaseIRException], NoJoin]
  with irarith.interpreter.ConcreteInterpreter
  with irstr.interpreter.ConcreteInterpreter
  with irdata.interpreter.ConcreteInterpreter:

  type CRV = ConcreteRelation[Value]

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures

  override lazy val boolOps: BooleanOps[Boolean] = ConcreteBooleanOps

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

  override val relationOps: RelationOps[Value, Boolean, CRV] = new ConcreteRelationOps[Value]


  fix.Fixpoint.DEBUG = false

  override val fixpoint: EffectStack ?=> fix.Fixpoint[FixIn, FixOut[Value, CRV]] =
    /*val fixPt =
      fix.notContextSensitive[FixIn, FixOut[Value, CRV], fix.Combinator[FixIn, FixOut[Value, CRV]]](
        fix.filter({
          case _: FixIn.EnterRelation => true
          case _ => false // important, filter everything out we don't need
        }, fix.iter.innermost[FixIn, FixOut[Value, CRV], Unit](StackedStates()))
          //fix.iter.innermost[FixIn, FixOut[Value, TRV], Unit](StackedCfgNodes()))
          //fix.iter.outermost[FixIn, FixOut[Value, TRV], Unit, Unit, Unit, Unit](StackedStates(readPriorOutput = true)))
        )*/

    // FIXME: Parameters are finite, but their values are not finite. However, if we assign an infinite set of values,
    //  then the Datalog program does not terminate.
    given Finite[Value] = new FiniteV
    val fixPt = fix.contextSensitive(
      fix.context.parameters[FixIn, String, Seq[Value]] {
        case FixIn.EnterRelation(r, adorn) =>
          val sup = supplementaryTable.getTable
          val paramMap = sup.cols.zipWithIndex.map { case (c, idx) =>
            c -> sup.rows.toSeq.map(_.apply(idx))
          }.toMap
          Some(paramMap)
        case _ => None
      },
      fix.filter({
        case _: FixIn.EnterRelation => true
        case _ => false // important, filter everything out we don't need
      }, fix.iter.innermost[FixIn, FixOut[Value, CRV], Parameters[String, Seq[Value]]](
        // TODO: Why is it incorrect to read prior output?
        //   Why this complex set intersection not terminate anymore with this set to true?
        StackedStates(readPriorOutput = false)
        //StackedStates()
        //StackedCfgNodes()
      ))
    )

    if (enableLogging)
      fix.log(new PrintLogger, fixPt).fixpoint
    else
      fixPt.fixpoint



