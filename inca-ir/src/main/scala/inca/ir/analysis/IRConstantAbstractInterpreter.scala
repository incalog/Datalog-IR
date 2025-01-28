package inca.ir.analysis

import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{BaseJoinV, ConstantRelation, ConstantRelationOps, Value}
import inca.ir.analysis.base.interpreter.{ASupplementaryTable, BaseGenericInterpreter, FixIn, FixOut, SupColumn}
import inca.ir.analysis.base.logger.{BaseAnalysisAnnotator, ControlEventLogger, DatalogControlObservable, PrintLogger}
import inca.ir.analysis.base.ordering.{BaseAtomOrderingOps, BaseEqOps}
import inca.ir.extension.arithmetic.analysis as irarith
import inca.ir.extension.data.analysis as irdata
import inca.ir.extension.string.analysis as irstr
import inca.ir.extension.aggregate.analysis as iragg
import sturdy.control.ControlEventGraphBuilder
import sturdy.data.WithJoin
import sturdy.values.{Changed, Join, MaybeChanged, Powerset, Topped, Widen}
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.effect.failure.{CollectedFailures, ObservableFailure}
import sturdy.fix
import sturdy.effect.except.{Except, JoinedExcept}
import sturdy.fix.{HasFixpointCache, StackConfig}
import sturdy.fix.StackConfig.StackedStates
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
  with irdata.interpreter.ConstantJoinV:

  override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
    val joined = join(v1, v2)
    if v1 == joined then
      Unchanged(joined)
    else
      Changed(joined)

private class IREqOps extends BaseEqOps
  with irarith.interpreter.ConstantEqOps
  with irstr.interpreter.ConstantEqOps
  with irdata.interpreter.ConstantEqOps

class IRConstantAbstractInterpreter(
    val logTraversalTrace: Boolean = false,
    val logControlEvents: Boolean = false,
    override val interRelational: Boolean = false
  )
  extends BaseGenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]
  with irarith.interpreter.ConstantAbstractInterpreter
  with irstr.interpreter.ConstantAbstractInterpreter
  with irdata.interpreter.ConstantAbstractInterpreter
  with iragg.interpreter.ConstantAbstractInterpreter
  with DatalogControlObservable:

  type RV = ConstantRelation

  private class IRAtomOrderingOps extends BaseAtomOrderingOps
    with irdata.ordering.AtomOrderingOps
  
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

  override val joinV: WithJoin[Value] = implicitly
  override val joinRV: Join[RV] = implicitly
  override val joinUnit: WithJoin[Unit] = implicitly

  // I don't think we need to widen tables for a constant analysis
  given Widen[RV] with {
    override def apply(v1: RV, v2: RV): MaybeChanged[RV] = joinRV(v1, v2)
  }
  
  override lazy val supplementaryTable: SupplementaryTable[ConstantRelation] = new ASupplementaryTable[RV]() {
    override def initialTable: RV = ConstantRelation(Seq(), Seq(), Topped.Actual(false))
  }
  override val relationOps: RelationOps[Value, Topped[Boolean], RV] = new ConstantRelationOps(using except)

  class AnalysisAnnotator
    extends BaseAnalysisAnnotator[Value, RV, Value]
      with irarith.logger.AnalysisAnnotator[Value, RV, Value]
      with irdata.logger.AnalysisAnnotator[Value, RV, Value]
      with irstr.logger.AnalysisAnnotator[Value, RV, Value]:

    override def extractTermValue(supName: SupColumn): Option[Value] =
      val supTable = supplementaryTable.getTable
      val termTRV = relationOps.project(supTable, Seq(supName))
      termTRV match
        case ConstantRelation.Empty(cs) => None
        case ConstantRelation.NonEmpty(cs, rows, emp) => 
          assert(rows.size == 1)
          Some(rows.head)

  // annotate information about constants
  val analysisAnnotator = new AnalysisAnnotator

  // log the control-flow graph
  private lazy val cfgLogger = new ControlEventLogger[Value, RV](this)

//  fix.Fixpoint.DEBUG = false

  //(new PrintingControlObserver()(println))
  val graphBuilder: ControlEventGraphBuilder[Int, SupColumn, BaseIRException, (FixIn, List[Any])] = addControlObserver(new ControlEventGraphBuilder)

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

  override val fixpoint: EffectStack ?=> fix.Fixpoint[FixIn, FixOut[Value, RV]] =
    var fixPt =
        fix.log(analysisAnnotator,
          fix.notContextSensitive[FixIn, FixOut[Value, RV], fix.Combinator[FixIn, FixOut[Value, RV]]](
            fix.filter(_.isInstanceOf[FixIn.EnterRelation],
              setLooper(fix.iter.topmost[FixIn, FixOut[Value, RV], Unit](stackConfig))
            )
          )
        )

    if (logControlEvents)
      fixPt = fix.log(cfgLogger, fixPt)
    if (logTraversalTrace)
      fixPt = fix.log(new PrintLogger, fixPt)

    fixPt.fixpoint