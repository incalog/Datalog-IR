package inca.ir.analysis

import inca.ir
import inca.ir.ExtensionalRelation
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.{ASupplementaryTable, BaseGenericInterpreter, FixIn, FixOut, SupColumn, given}
import inca.ir.analysis.base.logger.{BaseAnalysisLogger, PrintLogger}
import inca.ir.analysis.base.values.*
import inca.ir.extension.arithmetic.analysis as irarith
import inca.ir.extension.data.analysis as irdata
import inca.ir.extension.string.analysis as irstr
import sturdy.data.MayJoin.WithJoin
import sturdy.effect.EffectStack
import sturdy.effect.except.{Except, JoinedExcept}
import sturdy.effect.failure.CollectedFailures
import sturdy.effect.store.AStoreThreaded
import sturdy.fix
import sturdy.fix.StackConfig.StackedStates
import sturdy.values.*
import sturdy.values.booleans.{BooleanBranching, BooleanOps, ConcreteBooleanBranching, ToppedBooleanBranching}
import sturdy.values.ordering.EqOps
import sturdy.values.references.{AllocationSiteAddr, given_Finite_AllocationSiteAddr}

// Implicits
import inca.ir.analysis.base.effect.{IRException, IRFailure}
import inca.ir.analysis.base.interpreter.FiniteFixIn
import inca.ir.analysis.base.values.JoinTRV
import inca.ir.analysis.base.values.JoinTV
import sturdy.data.{MakeJoined, given}
import sturdy.values.exceptions.PowersetExceptional
import sturdy.values.given


class IRTypeAbstractInterpreter(val enableLogging: Boolean = false)
  extends BaseGenericInterpreter[TypeValue, Topped[Boolean], TypeRelation, Powerset[BaseIRException], WithJoin]
  with irarith.interpreter.TypeAbstractInterpreter
  with irstr.interpreter.TypeAbstractInterpreter
  with irdata.interpreter.TypeAbstractInterpreter:

  type TRV = TypeRelation

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures

  override val boolOps: BooleanOps[Topped[Boolean]] = implicitly

  override lazy val except: Except[BaseIRException, Powerset[BaseIRException], WithJoin] = new JoinedExcept(using PowersetExceptional[BaseIRException])

  given BooleanOps[Topped[Boolean]] = boolOps

  override val branchOps: BooleanBranching[Topped[Boolean], TRV] = new ToppedBooleanBranching[Boolean, TRV]

  override lazy val eqOps: EqOps[TypeValue, Topped[Boolean]] = new EqOps[TypeValue, Topped[Boolean]] {
    def equ(v1: TypeValue, v2: TypeValue): Topped[Boolean] = (v1, v2) match
      case (TypeValue.AType(ty1), TypeValue.AType(ty2)) => Topped.Actual(ty1 == ty2)
      case (TypeValue.Bottom, TypeValue.Bottom) => Topped.Actual(true)
      case (TypeValue.Bottom, _) => Topped.Actual(false)
      case (_, TypeValue.Bottom) => Topped.Actual(false)
      case (TypeValue.Top, TypeValue.Top) => Topped.Top
      case (TypeValue.Top, _) => Topped.Top
      case (_, TypeValue.Top) => Topped.Top

    def neq(v1: TypeValue, v2: TypeValue): Topped[Boolean] = (v1, v2) match
      case (TypeValue.AType(ty1), TypeValue.AType(ty2)) => Topped.Actual(ty1 == ty2)
      case (TypeValue.Bottom, TypeValue.Bottom) => Topped.Actual(false)
      case (TypeValue.Bottom, _) => Topped.Actual(true)
      case (_, TypeValue.Bottom) => Topped.Actual(true)
      case (TypeValue.Top, TypeValue.Top) => Topped.Top
      case (TypeValue.Top, _) => Topped.Top
      case (_, TypeValue.Top) => Topped.Top
  }
  
  override val joinV: WithJoin[TypeValue] = implicitly
  override val joinRV: Join[TRV] = implicitly
  
  // TODO: Do we need widening? If so, how does it look like?
  given Widen[TRV] with {
    override def apply(v1: TRV, v2: TRV): MaybeChanged[TRV] = joinRV(v1, v2)
  }
  
  override val joinUnit: WithJoin[Unit] = implicitly

  override val supplementaryTable: SupplementaryTable[TRV] = new ASupplementaryTable[TRV]() {
    override def initialTable: TRV = TypeRelation(Seq(), Seq(), Topped.Actual(false))
  }

  override val idb: AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, TRV] = AStoreThreaded[AllocationSiteAddr, AllocationSiteAddr, TRV](Map())

  override def resetIDB(): Unit = idb.setState(Map())

  given EqOps[TypeValue, Topped[Boolean]] = eqOps

  override val relationOps: RelationOps[TypeValue, Topped[Boolean], TypeRelation] = new TypeRelationOps

  class AnalysisLogger
    extends BaseAnalysisLogger[TypeValue, TRV, TypeValue]
    with irarith.logger.AnalysisLogger[TypeValue, TRV, TypeValue]
    with irdata.logger.AnalysisLogger[TypeValue, TRV, TypeValue]
    with irstr.logger.AnalysisLogger[TypeValue, TRV, TypeValue]:

      override def extractTermValue(supName: SupColumn): TypeValue =
        val supTable = supplementaryTable.getTable
        val termTRV = supTable.project(Seq(supName))
        assert(termTRV.rows.size == 1)
        termTRV.rows.head

  val analysisLogger: AnalysisLogger = new AnalysisLogger


  fix.Fixpoint.DEBUG = false

  override val fixpoint: EffectStack ?=> fix.Fixpoint[FixIn, FixOut[TypeValue, TRV]] =
    val fixPt =
      fix.notContextSensitive[FixIn, FixOut[TypeValue, TRV], fix.Combinator[FixIn, FixOut[TypeValue, TRV]]](
        fix.filter({
          case _: FixIn.EnterRelation => true
          case _ => false // important, filter everything out we don't need
        }, fix.iter.innermost[FixIn, FixOut[TypeValue, TRV], Unit](StackedStates()))
        )

    val analysisFixPt = fix.log(analysisLogger, fixPt)

    if (enableLogging)
      fix.log(new PrintLogger, analysisFixPt).fixpoint
    else
      analysisFixPt.fixpoint



