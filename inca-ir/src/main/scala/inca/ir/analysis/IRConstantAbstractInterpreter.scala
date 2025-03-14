package inca.ir.analysis

import inca.ir
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, ConcreteRelation, ConstantRelation, ConstantRelationOps, Meet, Value}
import inca.ir.analysis.base.interpreter.{ASupplementaryTable, BaseGenericInterpreter, FixIn, FixOut, SupColumn}
import inca.ir.analysis.base.logger.{BaseAnalysisAnnotator, ControlEventLogger, DatalogControlObservable, PrintLogger}
import inca.ir.analysis.base.ordering.{BaseAtomOrderingOps, BaseEqOps}
import inca.ir.analysis.constant.ConstantInterpreter
import inca.ir.extension.arithmetic.analysis as irarith
import inca.ir.extension.data.analysis as irdata
import inca.ir.extension.string.analysis as irstr
import inca.ir.extension.aggregate.analysis as iragg
import inca.ir.extension.tuple.analysis as irtuple
import inca.ir.extension.bool.analysis as irbool
import inca.ir.extension.demand.analysis as irdemand
import inca.ir.extension.not.analysis as irnot
import inca.ir.extension.disjunction.analysis as irdisjcuntion
import inca.ir.extension.block.analysis as irblock
import inca.ir.extension.datamatch.analysis as irdatamatch
import inca.ir.extension.set.analysis as irset
import inca.ir.extension.map.analysis as irmap
import inca.ir.extension.impure.analysis as irimpure
import sturdy.control.ControlEventGraphBuilder
import sturdy.data.{MayJoin, WithJoin}
import sturdy.values.{Changed, Finite, Join, MaybeChanged, Powerset, Topped, Widen}
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.effect.failure.{CollectedFailures, ObservableFailure}
import sturdy.fix
import sturdy.effect.except.{Except, JoinedExcept}
import sturdy.fix.{HasFixpointCache, StackConfig}
import sturdy.fix.StackConfig.StackedStates
import sturdy.fix.context.FiniteParameters
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.booleans.{BooleanBranching, BooleanOps, ToppedBooleanBranching, ToppedBooleanOps}
import sturdy.values.ordering.EqOps

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

private class IRJoinV extends Join[Value] with BaseJoinV
  with irarith.interpreter.ConstantJoinV
  with irstr.interpreter.ConstantJoinV
  with irdata.interpreter.ConstantJoinV
  with irtuple.interpreter.ConstantJoinV
  with irbool.interpreter.ConstantJoinV
  with irdemand.interpreter.ConstantJoinV
  with irnot.interpreter.ConstantJoinV
  with irdisjcuntion.interpreter.ConstantJoinV
  with irblock.interpreter.ConstantJoinV
  with irdatamatch.interpreter.ConstantJoinV
  with irset.interpreter.ConstantJoinV
  with irmap.interpreter.ConstantJoinV
  with irimpure.interpreter.ConstantJoinV:

  override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
    MaybeChanged(join(v1, v2), v1)

private class IRMeetV(using except: Except[BaseIRException, ?, ?]) extends BaseMeetV(using except)
  with irarith.interpreter.ConstantMeetV
  with irstr.interpreter.ConstantMeetV
  with irdata.interpreter.ConstantMeetV
  with irtuple.interpreter.ConstantMeetV
  with irbool.interpreter.ConstantMeetV
  with irdemand.interpreter.ConstantMeetV
  with irnot.interpreter.ConstantMeetV
  with irdisjcuntion.interpreter.ConstantMeetV
  with irblock.interpreter.ConstantMeetV
  with irdatamatch.interpreter.ConstantMeetV
  with irset.interpreter.ConstantMeetV
  with irmap.interpreter.ConstantMeetV
  with irimpure.interpreter.ConstantMeetV

private class IREqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps
  with irarith.interpreter.ConstantEqOps
  with irstr.interpreter.ConstantEqOps
  with irdata.interpreter.ConstantEqOps(using boolOps)
  with irtuple.interpreter.ConstantEqOps(using boolOps)
  with irbool.interpreter.ConstantEqOps(using boolOps)
  with irdemand.interpreter.ConstantEqOps
  with irnot.interpreter.ConstantEqOps
  with irdisjcuntion.interpreter.ConstantEqOps
  with irblock.interpreter.ConstantEqOps
  with irdatamatch.interpreter.ConstantEqOps
  with irset.interpreter.ConstantEqOps
  with irmap.interpreter.ConstantEqOps
  with irimpure.interpreter.ConstantEqOps

class IRConstantAbstractInterpreter(
    val logTraversalTrace: Boolean = false,
    val logControlEvents: Boolean = false,
    override val interRelational: Boolean = false
  )
  extends BaseGenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]
    with ConstantInterpreter
    with irarith.interpreter.ConstantAbstractInterpreter
    with irstr.interpreter.ConstantAbstractInterpreter
    with irdata.interpreter.ConstantAbstractInterpreter
    with iragg.interpreter.ConstantAbstractInterpreter
    with irtuple.interpreter.ConstantAbstractInterpreter
    with irbool.interpreter.ConstantAbstractInterpreter
    with irdemand.interpreter.ConstantAbstractInterpreter
    with irnot.interpreter.ConstantAbstractInterpreter
    with irdisjcuntion.interpreter.ConstantAbstractInterpreter
    with irblock.interpreter.ConstantAbstractInterpreter
    with irdatamatch.interpreter.ConstantAbstractInterpreter
    with irset.interpreter.ConstantAbstractInterpreter
    with irmap.interpreter.ConstantAbstractInterpreter
    with irimpure.interpreter.ConstantAbstractInterpreter
    with DatalogControlObservable:

  type RV = ConstantRelation

  private class IRAtomOrderingOps extends BaseAtomOrderingOps
    with irarith.ordering.AtomOrderingOps
    with irstr.ordering.AtomOrderingOps
    with iragg.ordering.AtomOrderingOps
    with irdata.ordering.AtomOrderingOps
    with irtuple.ordering.AtomOrderingOps
    with irbool.ordering.AtomOrderingOps
    with irdemand.ordering.AtomOrderingOps
    with irnot.ordering.AtomOrderingOps
    with irdisjcuntion.ordering.AtomOrderingOps
    with irblock.ordering.AtomOrderingOps
    with irdatamatch.ordering.AtomOrderingOps
    with irset.ordering.AtomOrderingOps
    with irmap.ordering.AtomOrderingOps
    with irimpure.ordering.AtomOrderingOps
  
  override val atomOrderingOps = new IRAtomOrderingOps
  
  override lazy val topV: Value = Value.Top

  override lazy val except: Except[BaseIRException, Powerset[BaseIRException], WithJoin] = new JoinedExcept(using PowersetExceptional[BaseIRException])

  override val branchOps: BooleanBranching[Topped[Boolean], RV] = new ToppedBooleanBranching[Boolean, RV]

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures with ObservableFailure(this)

  override lazy val boolOps: BooleanOps[Topped[Boolean]] = ToppedBooleanOps

  given BooleanOps[Topped[Boolean]] = boolOps

  override lazy val eqOps: BaseEqOps = new IREqOps

  given EqOps[Value, Topped[Boolean]] = eqOps

  given Join[Value] = new IRJoinV

  override val mayJoinV: WithJoin[Value] = implicitly
  override val joinRV: Join[RV] = implicitly
  override val mayJoinUnit: WithJoin[Unit] = implicitly
  override lazy val mayJoinRV: MayJoin.WithJoin[ConstantRelation] = MakeJoined(using joinRV, effects)

  // I don't think we need to widen tables for a constant analysis
  given Widen[RV] with {
    override def apply(v1: RV, v2: RV): MaybeChanged[RV] = joinRV(v1, v2)
  }
  
  override lazy val supplementaryTable: SupplementaryTable[ConstantRelation] = new ASupplementaryTable[RV]() {
    override def initialTable: RV = ConstantRelation(Seq(), Seq(), Topped.Actual(false))
  }

  given Meet[Value] = IRMeetV(using except)
  override val relationOps: RelationOps[Value, Topped[Boolean], RV] = new ConstantRelationOps(using except)


  class AnalysisAnnotator
    extends BaseAnalysisAnnotator[Value, RV, Value]
      with irarith.logger.AnalysisAnnotator[Value, RV, Value]
      with irdata.logger.AnalysisAnnotator[Value, RV, Value]
      with irstr.logger.AnalysisAnnotator[Value, RV, Value]
      with iragg.logger.AnalysisAnnotator[Value, RV, Value]
      with irtuple.logger.AnalysisAnnotator[Value, RV, Value]
      with irbool.logger.AnalysisAnnotator[Value, RV, Value]
      with irdemand.logger.AnalysisAnnotator[Value, RV, Value]
      with irnot.logger.AnalysisAnnotator[Value, RV, Value]
      with irdisjcuntion.logger.AnalysisAnnotator[Value, RV, Value]
      with irblock.logger.AnalysisAnnotator[Value, RV, Value]
      with irdatamatch.logger.AnalysisAnnotator[Value, RV, Value]
      with irset.logger.AnalysisAnnotator[Value, RV, Value]
      with irmap.logger.AnalysisAnnotator[Value, RV, Value]
      with irimpure.logger.AnalysisAnnotator[Value, RV, Value]:

    override def extractColumns(rv: RV): Seq[String] =
      relationOps.columns(rv)
      
    override def extractTermValue(supName: SupColumn, rv: RV): Option[Value] =
      if (relationOps.hasColumn(rv, supName))
        val termTRV = relationOps.project(rv, Seq(supName))
        termTRV match
          case ConstantRelation.Empty(cs) => None
          case ConstantRelation.NonEmpty(cs, rows, emp) =>
            assert(rows.size == 1)
            Some(rows.head)
      else
        None

  // annotate information about constants
  val analysisAnnotator = new AnalysisAnnotator

  // log the control-flow graph
  private lazy val cfgLogger = new ControlEventLogger[Value, RV](this)

  //fix.Fixpoint.DEBUG = true

  //(new PrintingControlObserver()(println))
  val graphBuilder: ControlEventGraphBuilder[Long, Long, BaseIRException, (FixIn, List[Any])] = addControlObserver(new ControlEventGraphBuilder)

  private val stackConfig: StackConfig = if (logControlEvents)
    StackedStates(storeNonrecursiveOutput = true).withObservers(Seq(triggerControlEvent))
  else
    StackedStates(storeNonrecursiveOutput = true)

  var looper: HasFixpointCache[FixIn, FixOut[Value, RV]] = null
  def setLooper[A <: HasFixpointCache[FixIn, FixOut[Value, RV]]](a: A): A =
    looper = a
    a
  override def getIDB: Map[String, RV] =
    val collected = looper.getCache.collect {
      case (FixIn.EnterRelation(rel, adorn), TrySturdy.Success(FixOut.Relation(rv))) => (rel.name.name, adorn) -> rv
    }
    val reduced = collected.groupBy(_._1._1).view.mapValues { m =>
      m.values.reduce((r1,r2) => Join(r1,r2).get)
    }.toMap
    reduced

//  type Ctx = fix.context.Parameters[String, ValueKind]
//  private val parameters: fix.context.Sensitivity[FixIn, Ctx] = fix.context.parameters { _ =>
//    Some(supplementaryTable.getTable match
//      case ConstantRelation.Empty(_) => Map()
//      case ConstantRelation.NonEmpty(_, _, Topped.Actual(true)) => Map()
//      case ConstantRelation.NonEmpty(cs, rs, _) => cs.zip(rs.map(getValueKind)).toMap
//    )
//  }
  type Ctx = Unit
  override val fixpoint: EffectStack ?=> fix.Fixpoint[FixIn, FixOut[Value, RV]] =
    var fixPt =
        fix.log(analysisAnnotator,
          fix.filter({case _: FixIn.EnterRelation => true; case _ => false},
//            fix.contextSensitive(
//              parameters,
          fix.notContextSensitive[FixIn, FixOut[Value, RV], fix.Combinator[FixIn, FixOut[Value, RV]]](
              setLooper(fix.iter.topmost[FixIn, FixOut[Value, RV], Ctx](stackConfig))
              //setLooper(fix.iter.outermost[FixIn, FixOut[Value, RV], Ctx](stackConfig))
            )
          )
        )

    if (logControlEvents)
      fixPt = fix.log(cfgLogger, fixPt)
    if (logTraversalTrace)
      fixPt = fix.log(new PrintLogger, fixPt)

    fixPt.fixpoint