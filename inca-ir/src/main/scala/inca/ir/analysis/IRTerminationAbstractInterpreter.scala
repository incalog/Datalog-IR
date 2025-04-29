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
import inca.ir.extension.datamatch.analysis as irdatamatch
import inca.ir.extension.demand.analysis as irdemand
import inca.ir.extension.disjunction.analysis as irdisjcuntion
import inca.ir.extension.impure.analysis as irimpure
import inca.ir.extension.map.analysis as irmap
import inca.ir.extension.not.analysis as irnot
import inca.ir.extension.set.analysis as irset
import inca.ir.extension.string.analysis as irstr
import inca.ir.extension.tuple.analysis as irtuple
import inca.ir.optimize.{AbstractEdbConfig, EdbConfig, Optimizer}
import inca.ir.printer.IRDebugPrinter
import inca.ir.visitors.{BaseIRVisitor, IRVisitor}
import sturdy.control.ControlEventGraphBuilder
import sturdy.data.{MayJoin, WithJoin}
import sturdy.effect.except.{Except, JoinedExcept}
import sturdy.effect.failure.{AFallible, CollectedFailures, ObservableFailure}
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.fix
import sturdy.fix.StackConfig.StackedStates
import sturdy.fix.context.FiniteParameters
import sturdy.fix.{HasFixpointCache, StackConfig}
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.booleans.{BooleanBranching, BooleanOps, ToppedBooleanBranching, ToppedBooleanOps}
import sturdy.values.ordering.EqOps
import sturdy.values.*

// Implicits
import inca.ir.analysis.base.effect.{IRException, IRFailure}
import inca.ir.analysis.base.interpreter.{CCombineFixOut, FiniteFixIn}
import inca.ir.analysis.base.values.{JoinRV, WidenRV}
import sturdy.data.given
import sturdy.values.booleans.ConcreteBooleanBranching
import sturdy.values.exceptions.PowersetExceptional
import sturdy.values.given

/**
 * An extensible constant analysis.
 */
class IRTerminationAbstractInterpreter(
    val logTraversalTrace: Boolean = false,
    val logControlEvents: Boolean = false,
    override val interRelational: Boolean = false
  )
  extends BaseGenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]
    with irarith.interpreter.IntervalAbstractInterpreter
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

  type RV = AbstractRelation

  private class IRJoinV extends Join[Value] with BaseJoinV
    with irarith.interpreter.IntervalJoinV
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

  private class IRWidenV extends Widen[Value] with BaseJoinV
    with irarith.interpreter.IntervalWidenV
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
    with irarith.interpreter.IntervalJoinV
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
    with irarith.interpreter.IntervalEqOps
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
  override lazy val mayJoinRV: MayJoin.WithJoin[AbstractRelation] = MakeJoined(using joinRV, effects)

  given Widen[Value] = new IRWidenV
  given Widen[RV] = new WidenRV
  
  override lazy val supplementaryTable: SupplementaryTable[AbstractRelation] = new AbstractSupplementaryTable[RV]() {
    override def initialTable: RV = AbstractRelation(Seq(), Seq(), Topped.Actual(false))
  }

  given Meet[Value] = IRMeetV(using except)
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
//      case AbstractRelation.Empty(_) => Map()
//      case AbstractRelation.NonEmpty(_, _, Topped.Actual(true)) => Map()
//      case AbstractRelation.NonEmpty(cs, rs, _) => cs.zip(rs.map(getValueKind)).toMap
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


case class AnalysisFailed(msg: String) extends Exception:
  override def toString: String = msg


class IRTerminationAnalysis extends IRVisitor with Optimizer:
  // Configure
  val edbConfig: EdbConfig[AbstractRelation] = AbstractEdbConfig.default

  val abstractInterpreter: BaseGenericInterpreter[Value, ?, AbstractRelation, ?, ?] =
    new IRTerminationAbstractInterpreter(false, false, true)

  private var analysisHasRun: Boolean = false

  override def analyzeProgram(modules: Seq[ir.Module]): Unit =
    analysisHasRun = true

    // Fill edb
    modules.foreach { m =>
      m.entries.foreach {
        case (_, ir.ExtensionalRelation(n, params)) =>
          val aRel = edbConfig.abstractExtensionalRelation(n, params)
          abstractInterpreter.insertEDB(n.name, aRel)
        case _ => // nothing
      }
    }

    // Analyse
    val analysisRes = abstractInterpreter.failure.fallible {
      abstractInterpreter.evalProgram(modules)
    }

    println(new IRDebugPrinter{}.prettyPrint(modules))
    System.exit(1)

    // Interpret result
    analysisRes match {
      case AFallible.Failing(failures) =>
        val msg = failures.map { (kind, message) =>
          s"[$kind]: $message"
        }.set.mkString("\n")
        throw AnalysisFailed(msg)
      case AFallible.Diverging(recur) =>
        throw IllegalStateException()
      case _ => // nothing
    }

  override def visitProgram(modules: Seq[ir.Module], dependencies: Seq[ir.Module]): Seq[ir.Module] =
    if (!analysisHasRun)
      analyzeProgram(modules)
    super.visitProgram(modules, dependencies)