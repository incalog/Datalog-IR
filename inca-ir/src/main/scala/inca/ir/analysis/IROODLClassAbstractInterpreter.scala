package inca.ir.analysis

import inca.ir
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.*
import inca.ir.analysis.base.logger.{BaseAnalysisAnnotator, ControlEventLogger, DatalogControlObservable, PrintLogger}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.*
import inca.ir.extension.aggregate.analysis as iragg
import inca.ir.extension.arithmetic.analysis as irarith
import inca.ir.extension.block.analysis as irblock
import inca.ir.extension.bool.analysis as irbool
import inca.ir.extension.data.analysis as irdata
import inca.ir.extension.data.analysis.interpreter.{ClassOps, OODLClassV}
import inca.ir.extension.datamatch.analysis as irdatamatch
import inca.ir.extension.demand.analysis as irdemand
import inca.ir.extension.disjunction.analysis as irdisjcuntion
import inca.ir.extension.impure.analysis as irimpure
import inca.ir.extension.map.analysis as irmap
import inca.ir.extension.not.analysis as irnot
import inca.ir.extension.set.analysis as irset
import inca.ir.extension.string.analysis as irstr
import inca.ir.extension.tuple.analysis as irtuple
import inca.util.MapUtil
import sturdy.control.ControlEventGraphBuilder
import sturdy.data.{MayJoin, WithJoin}
import sturdy.effect.except.{Except, JoinedExcept}
import sturdy.effect.failure.{CollectedFailures, ObservableFailure}
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.fix
import sturdy.fix.StackConfig.StackedStates
import sturdy.fix.context.FiniteParameters
import sturdy.fix.{HasFixpointCache, StackConfig}
import sturdy.values.*
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.booleans.{BooleanBranching, BooleanOps, ToppedBooleanBranching, ToppedBooleanOps}
import sturdy.values.ordering.EqOps

import scala.annotation.tailrec

// Implicits
import inca.ir.analysis.base.effect.{IRException, IRFailure}
import inca.ir.analysis.base.interpreter.{CCombineFixOut, FiniteFixIn}
import inca.ir.analysis.base.values.JoinRV
import sturdy.data.given
import sturdy.values.booleans.ConcreteBooleanBranching
import sturdy.values.exceptions.PowersetExceptional
import sturdy.values.given

class IROODLClassAbstractInterpreter(
                                      var superClassMap: Map[String, Set[String]], // cls -> direct superclass of cls
                                      val logTraversalTrace: Boolean = false,
                                      val logControlEvents: Boolean = false,
                                      override val interRelational: Boolean = false
  )
  extends BaseGenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]
    with irarith.interpreter.ConstantAbstractInterpreter
    with irstr.interpreter.ConstantAbstractInterpreter
    with irdata.interpreter.OODLClassAbstractInterpreter // specific to OODL
    with iragg.interpreter.ConstantAbstractInterpreter
    with irtuple.interpreter.ConstantAbstractInterpreter
    with irbool.interpreter.ConstantAbstractInterpreter
    with irdemand.interpreter.OODLClassAbstractInterpreter // specific to OODL
    with irnot.interpreter.ConstantAbstractInterpreter
    with irdisjcuntion.interpreter.ConstantAbstractInterpreter
    with irblock.interpreter.ConstantAbstractInterpreter
    with irdatamatch.interpreter.ConstantAbstractInterpreter
    with irset.interpreter.BoundedAbstractInterpreter
    with irmap.interpreter.BoundedAbstractInterpreter
    with irimpure.interpreter.ConstantAbstractInterpreter
    with DatalogControlObservable
    with RequireMeet[Value]
    with RequireJoin[Value]:

  type RV = AbstractRelation

  // all ancestors for a given class
  lazy val ancestors: Map[String, Set[String]] =
    MapUtil.transClosure(superClassMap)

  lazy val descendants: Map[String, Set[String]] =
    val subclasses = superClassMap.toSeq.flatMap((k, vs) => vs.toSeq.map(_ -> k))
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).toSet)
      .toMap
    MapUtil.transClosure(subclasses)

  given classOps: ClassOps[OODLClassV, Boolean] with
    override def isSubclass(cls1: OODLClassV, cls2: OODLClassV): Boolean = (cls1, cls2) match
      case (OODLClassV(clsName1, _), OODLClassV(clsName2, _)) =>
        if (clsName1 == clsName2) true
        else if (descendants.getOrElse(clsName2, Set()).contains(clsName1)) true
        else false

    override def join(cls1: OODLClassV, cls2: OODLClassV): OODLClassV = (cls1, cls2) match
      case (OODLClassV(clsName1, isRuntimeType1), OODLClassV(clsName2, isRuntimeType2)) =>
        if (clsName1 == clsName2)
          OODLClassV(clsName1, isRuntimeType1 && isRuntimeType2)
        else if (descendants.getOrElse(clsName2, Set()).contains(clsName1))
          OODLClassV(clsName2, false) // cls1 is subclass of cls2
        else if (descendants.getOrElse(clsName1, Set()).contains(clsName2))
          OODLClassV(clsName1, false) // cls2 is subclass of cls1
        else
          val commonAncestors = ancestors(clsName1).intersect(ancestors(clsName2))
          // Pick the most specific (i.e., lowest in the hierarchy)
          val lub = commonAncestors.find { candidate =>
            !commonAncestors.exists { other =>
              other != candidate && ancestors(other).contains(candidate)
            }
          }.get
          OODLClassV(lub, false)

    override def meet(cls1: OODLClassV, cls2: OODLClassV): OODLClassV = (cls1, cls2) match
      case (OODLClassV(clsName1, isRuntimeType1), OODLClassV(clsName2, isRuntimeType2)) =>
        if (clsName1 == clsName2)
          OODLClassV(clsName1, isRuntimeType1 || isRuntimeType2)
        else if (descendants.getOrElse(clsName2, Set()).contains(clsName1))
          cls1 // cls1 is subclass of cls2
        else if (descendants.getOrElse(clsName1, Set()).contains(clsName2))
          cls2 // cls2 is subclass of cls1
        else OODLClassV.Null


  private class IRJoinV extends Join[Value] with BaseJoinV
    with irarith.interpreter.ConstantJoinV
    with irstr.interpreter.ConstantJoinV
    with irdata.interpreter.OODLClassJoinV
    with irtuple.interpreter.ConstantJoinV
    with irbool.interpreter.ConstantJoinV
    with irdemand.interpreter.OODLClassJoinV
    with irnot.interpreter.ConstantJoinV
    with irdisjcuntion.interpreter.ConstantJoinV
    with irblock.interpreter.ConstantJoinV
    with irdatamatch.interpreter.ConstantJoinV
    with irset.interpreter.BoundedJoinV
    with irmap.interpreter.BoundedJoinV
    with irimpure.interpreter.ConstantJoinV:

    override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
      MaybeChanged(combine(v1, v2), v1)

  private class IRMeetV(using except: Except[BaseIRException, Powerset[BaseIRException], WithJoin]) extends BaseMeetV(using except)
    with irarith.interpreter.ConstantMeetV
    with irstr.interpreter.ConstantMeetV
    with irdata.interpreter.OODLClassMeetV
    with irtuple.interpreter.ConstantMeetV
    with irbool.interpreter.ConstantMeetV
    with irdemand.interpreter.OODLClassMeetV
    with irnot.interpreter.ConstantMeetV
    with irdisjcuntion.interpreter.ConstantMeetV
    with irblock.interpreter.ConstantMeetV
    with irdatamatch.interpreter.ConstantMeetV
    with irset.interpreter.BoundedMeetV[WithJoin]
    with irmap.interpreter.BoundedMeetV[WithJoin]
    with irimpure.interpreter.ConstantMeetV

  private class IREqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps
    with irarith.interpreter.ConstantEqOps
    with irstr.interpreter.ConstantEqOps
    with irdata.interpreter.OODLClassEqOps(using boolOps)
    with irtuple.interpreter.ConstantEqOps(using boolOps)
    with irbool.interpreter.ConstantEqOps(using boolOps)
    with irdemand.interpreter.OODLClassEqOps
    with irnot.interpreter.ConstantEqOps
    with irdisjcuntion.interpreter.ConstantEqOps
    with irblock.interpreter.ConstantEqOps
    with irdatamatch.interpreter.ConstantEqOps
    with irset.interpreter.BoundedEqOps
    with irmap.interpreter.BoundedEqOps
    with irimpure.interpreter.ConstantEqOps
  
  override lazy val topV: Value = Value.Top

  override lazy val except: Except[BaseIRException, Powerset[BaseIRException], WithJoin] = new JoinedExcept(using PowersetExceptional[BaseIRException])

  override val branchOps: BooleanBranching[Topped[Boolean], RV] = new ToppedBooleanBranching[Boolean, RV]

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures with ObservableFailure(this)

  override lazy val boolOps: BooleanOps[Topped[Boolean]] = ToppedBooleanOps

  given BooleanOps[Topped[Boolean]] = boolOps

  override lazy val eqOps: BaseEqOps = new IREqOps

  given EqOps[Value, Topped[Boolean]] = eqOps

  override val meetV: Meet[Value] = IRMeetV(using except)
  override val joinV: Join[Value] = IRJoinV()

  given Join[Value] = joinV

  override val mayJoinV: WithJoin[Value] = implicitly
  override val joinRV: Join[RV] = implicitly
  override val mayJoinUnit: WithJoin[Unit] = implicitly
  override lazy val mayJoinRV: MayJoin.WithJoin[AbstractRelation] = MakeJoined(using joinRV, effects)

  // I don't think we need to widen tables for a constant analysis
  given Widen[RV] with {
    override def apply(v1: RV, v2: RV): MaybeChanged[RV] = joinRV(v1, v2)
  }
  
  override lazy val supplementaryTable: SupplementaryTable[AbstractRelation] = new AbstractSupplementaryTable[RV]() {
    override def initialTable: RV = AbstractRelation(Seq(), Seq(), Topped.Actual(false))
  }

  given Meet[Value] = meetV

  override val relationOps: RelationOps[Value, Topped[Boolean], RV] = new AbstractRelationOps(using except)


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
          case AbstractRelation.Empty(cs) => None
          case AbstractRelation.NonEmpty(cs, rows, emp) =>
            assert(rows.size == 1)
            Some(rows.head)
      else
        None

  // annotate information about constants
  val analysisAnnotator = new AnalysisAnnotator

  // log the control-flow graph
  private lazy val cfgLogger = new ControlEventLogger[Value, RV](this)

  //fix.Fixpoint.DEBUG = true

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

  type Ctx = Unit

  override val fixpoint: EffectStack ?=> fix.Fixpoint[FixIn, FixOut[Value, RV]] =
    var fixPt =
        fix.log(analysisAnnotator,
          fix.filter({case _: FixIn.EnterRelation => true; case _ => false},
            fix.notContextSensitive[FixIn, FixOut[Value, RV], fix.Combinator[FixIn, FixOut[Value, RV]]](
                setLooper(fix.iter.topmost[FixIn, FixOut[Value, RV], Ctx](stackConfig))
              )
            )
        )

    if (logControlEvents)
      fixPt = fix.log(cfgLogger, fixPt)
    if (logTraversalTrace)
      fixPt = fix.log(new PrintLogger, fixPt)

    fixPt.fixpoint