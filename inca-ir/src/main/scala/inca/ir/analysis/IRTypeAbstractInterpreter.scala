package inca.ir.analysis

import inca.ir
import inca.ir.ExtensionalRelation
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.{ASupplementaryTable, BaseGenericInterpreter, FixIn, FixOut, SupColumn, given}
import inca.ir.analysis.base.logger.{BaseAnalysisAnnotator, PrintLogger}
import inca.ir.analysis.base.values.{TypeValue, *}
import inca.ir.extension.arithmetic.analysis as irarith
import inca.ir.extension.data.analysis as irdata
import inca.ir.extension.string.analysis as irstr
import inca.ir.extension.aggregate.analysis as iragg
import sturdy.data.MayJoin.WithJoin
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.effect.except.{Except, JoinedExcept}
import sturdy.effect.failure.CollectedFailures
import sturdy.effect.store.AStoreThreaded
import sturdy.fix
import sturdy.fix.HasFixpointCache
import sturdy.fix.StackConfig.StackedStates
import sturdy.values.*
import sturdy.values.booleans.{BooleanBranching, BooleanOps, ConcreteBooleanBranching, ToppedBooleanBranching, ToppedBooleanOps}
import sturdy.values.ordering.EqOps
import sturdy.values.references.{AllocationSiteAddr, given_Finite_AllocationSiteAddr}

// Implicits
import sturdy.values.booleans.ConcreteBooleanOps
import inca.ir.analysis.base.effect.{IRException, IRFailure}
import inca.ir.analysis.base.interpreter.FiniteFixIn
import inca.ir.analysis.base.values.{ JoinTV, JoinTRV }
import sturdy.data.{MakeJoined, given}
import sturdy.values.exceptions.PowersetExceptional
import sturdy.values.given


class IRTypeAbstractInterpreter(
     val enableLogging: Boolean = false,
     override val interRelational: Boolean = false
  )
  extends BaseGenericInterpreter[TypeValue, Topped[Boolean], TypeRelation, Powerset[BaseIRException], WithJoin]
  with irarith.interpreter.TypeAbstractInterpreter
  with irstr.interpreter.TypeAbstractInterpreter
  with irdata.interpreter.TypeAbstractInterpreter
  with iragg.interpreter.TypeAbstractInterpreter:

  type TRV = TypeRelation

  override lazy val topV: TypeValue = TypeValue.Top

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures

  override lazy val except: Except[BaseIRException, Powerset[BaseIRException], WithJoin] = new JoinedExcept(using PowersetExceptional[BaseIRException])

  override lazy val boolOps: BooleanOps[Topped[Boolean]] = new ToppedBooleanOps

  given BooleanOps[Topped[Boolean]] = boolOps

  override val branchOps: BooleanBranching[Topped[Boolean], TRV] = new ToppedBooleanBranching[Boolean, TRV]

  override lazy val eqOps: EqOps[TypeValue, Topped[Boolean]] = new EqOps[TypeValue, Topped[Boolean]] {
    def equ(v1: TypeValue, v2: TypeValue): Topped[Boolean] = (v1, v2) match
      case (TypeValue.AType(ty1), TypeValue.AType(ty2)) => if (ty1 != ty2) Topped.Actual(false) else Topped.Top
      case (TypeValue.Bottom, TypeValue.Bottom) => Topped.Actual(true)
      case (TypeValue.Bottom, _) => Topped.Actual(false)
      case (_, TypeValue.Bottom) => Topped.Actual(false)
      case (TypeValue.Top, TypeValue.Top) => Topped.Top
      case (TypeValue.Top, _) => Topped.Top
      case (_, TypeValue.Top) => Topped.Top

    def neq(v1: TypeValue, v2: TypeValue): Topped[Boolean] = (v1, v2) match
      case (TypeValue.AType(ty1), TypeValue.AType(ty2)) => if (ty1 != ty2) Topped.Actual(true) else Topped.Top
      case (TypeValue.Bottom, TypeValue.Bottom) => Topped.Actual(false)
      case (TypeValue.Bottom, _) => Topped.Actual(true)
      case (_, TypeValue.Bottom) => Topped.Actual(true)
      case (TypeValue.Top, TypeValue.Top) => Topped.Top
      case (TypeValue.Top, _) => Topped.Top
      case (_, TypeValue.Top) => Topped.Top
  }

  given EqOps[TypeValue, Topped[Boolean]] = eqOps
  
  override val joinV: WithJoin[TypeValue] = implicitly
  override val joinRV: Join[TRV] = implicitly
  
  given Widen[TRV] with {
    override def apply(v1: TRV, v2: TRV): MaybeChanged[TRV] = joinRV(v1, v2)
  }
  
  override val joinUnit: WithJoin[Unit] = implicitly

  override val supplementaryTable: SupplementaryTable[TRV] = new ASupplementaryTable[TRV]() {
    override def initialTable: TRV = TypeRelation(Seq(), Seq(), Topped.Actual(false))
  }

  override val relationOps: RelationOps[TypeValue, Topped[Boolean], TypeRelation] = new TypeRelationOps

  class AnalysisAnnotator
    extends BaseAnalysisAnnotator[TypeValue, TRV, TypeValue]
    with irarith.logger.AnalysisAnnotator[TypeValue, TRV, TypeValue]
    with irdata.logger.AnalysisAnnotator[TypeValue, TRV, TypeValue]
    with irstr.logger.AnalysisAnnotator[TypeValue, TRV, TypeValue]:

      override def extractTermValue(supName: SupColumn): Option[TypeValue] =
        val supTable = supplementaryTable.getTable
        val termTRV = supTable.project(Seq(supName))
        assert(termTRV.rows.size == 1)
        Some(termTRV.rows.head)

  val analysisAnnotator: AnalysisAnnotator = new AnalysisAnnotator
  
  //fix.Fixpoint.DEBUG = true

  var looper: HasFixpointCache[FixIn, FixOut[TypeValue, TRV]] = null
  def setLooper[A <: HasFixpointCache[FixIn, FixOut[TypeValue, TRV]]](a: A): A =
    looper = a
    a
  override def getIDB: Map[String, TRV] =
    val collected = looper.getCache.collect {
      case (FixIn.EnterRelation(rel, adorn), TrySturdy.Success(FixOut.Relation(rv))) => (rel.name.name, adorn) -> rv
    }
    val reduced = collected.groupBy(_._1._1).view.mapValues { m =>
      m.values.reduce((r1,r2) => Join(r1,r2).get)
    }.toMap
    reduced

  private val stackConfig = StackedStates()
  override val fixpoint: EffectStack ?=> fix.Fixpoint[FixIn, FixOut[TypeValue, TRV]] =
    val fixPt =
      fix.notContextSensitive[FixIn, FixOut[TypeValue, TRV], fix.Combinator[FixIn, FixOut[TypeValue, TRV]]](
        fix.filter({
          case _: FixIn.EnterRelation => true
          case _ => false // important, filter everything out we don't need
        }, setLooper(fix.iter.innermost[FixIn, FixOut[TypeValue, TRV], Unit](stackConfig)))
        )

    val analysisFixPt = fix.log(analysisAnnotator, fixPt)

    if (enableLogging)
      fix.log(new PrintLogger, analysisFixPt).fixpoint
    else
      analysisFixPt.fixpoint



