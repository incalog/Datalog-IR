package inca.ir.analysis

import inca.ir
import inca.ir.{Name, Param}
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.*
import inca.ir.analysis.base.logger.{BaseAnalysisAnnotator, ControlEventLogger, DatalogControlObservable, PrintLogger}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.*
import inca.ir.extension.arithmetic.analysis as irarith
import inca.ir.extension.data.analysis as irdata
import inca.ir.extension.string.analysis as irstr
import inca.ir.optimize.Optimizer
import inca.ir.visitors.IRVisitor
import sturdy.control.ControlEventGraphBuilder
import sturdy.data.{MakeJoined, MayJoin, WithJoin}
import sturdy.effect.except.{Except, JoinedExcept}
import sturdy.effect.failure.{AFallible, CollectedFailures, ObservableFailure}
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.fix
import sturdy.fix.StackConfig.StackedStates
import sturdy.fix.{Combinator, HasFixpointCache, StackConfig}
import sturdy.values.*
import sturdy.values.booleans.{BooleanBranching, BooleanOps, ConcreteBooleanBranching, ToppedBooleanBranching, ToppedBooleanOps}
import sturdy.values.exceptions.PowersetExceptional
import sturdy.values.ordering.EqOps

// Implicits
import inca.ir.analysis.base.effect.{IRException, IRFailure}
import inca.ir.analysis.base.interpreter.{CCombineFixOut, FiniteFixIn}
import inca.ir.analysis.base.values.{ProvenanceJoinRV, ProvenanceWidenRV}
import sturdy.data.given
import sturdy.values.given


class ProvenanceEdbConfig extends EdbConfig[ProvenanceAbstractRelation]:
  override def abstractExtensionalRelation(n: Name, params: Seq[Param]): ProvenanceAbstractRelation =
    val (aCols, aRows) = params.map { p =>
      val col = p.name.name
      col -> ProvenanceV(s"edb.${n.name}.$col")
    }.unzip
    ProvenanceAbstractRelation(aCols, aRows)

object ProvenanceEdbConfig:
  val default: ProvenanceEdbConfig = new ProvenanceEdbConfig


/**
 * An extensible provenance analysis.
 */
class IRProvenanceAbstractInterpreter(
    val logTraversalTrace: Boolean = false,
    val logControlEvents: Boolean = false,
    override val interRelational: Boolean = false
  )
  extends BaseGenericInterpreter[Value, Topped[Boolean], ProvenanceAbstractRelation, Powerset[BaseIRException], WithJoin]
    with irarith.interpreter.ProvenanceAbstractInterpreter
    with irstr.interpreter.ProvenanceAbstractInterpreter
    with irdata.interpreter.ProvenanceAbstractInterpreter
    with DatalogControlObservable:

  type RV = ProvenanceAbstractRelation

  private class IRJoinV extends Join[Value] with BaseJoinV:
    override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
      case (ProvenanceV(left), ProvenanceV(right)) => ProvenanceV(left ++ right)
      case _ => super.combine(lhs, rhs)

    override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
      MaybeChanged(combine(v1, v2), v1)

  // We don't need widening, since the domain of edb values is finit and the domain of runtime values is limited to the
  // available types.
  private class IRWidenV extends Widen[Value] with BaseWidenV:
    override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
      case (ProvenanceV(left), ProvenanceV(right)) => ProvenanceV(left ++ right)
      case _ => super.combine(lhs, rhs)

    override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
      MaybeChanged(combine(v1, v2), v1)

  private class IREqOps extends BaseEqOps:
    override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
      case (ProvenanceV(left), ProvenanceV(right)) if left == right => Topped.Actual(true)
      case _ => Topped.Top

    override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
      case (ProvenanceV(left), ProvenanceV(right)) if left == right => Topped.Actual(false)
      case _ => Topped.Top

  override lazy val topV: Value = ProvenanceV.empty

  override lazy val except: Except[BaseIRException, Powerset[BaseIRException], WithJoin] =
    new JoinedExcept(using PowersetExceptional[BaseIRException])

  override val branchOps: BooleanBranching[Topped[Boolean], RV] =
    new ToppedBooleanBranching[Boolean, RV]

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] =
    new CollectedFailures with ObservableFailure(this)

  override lazy val boolOps: BooleanOps[Topped[Boolean]] = ToppedBooleanOps

  given BooleanOps[Topped[Boolean]] = boolOps

  override lazy val eqOps: BaseEqOps = new IREqOps

  given EqOps[Value, Topped[Boolean]] = eqOps

  given Join[Value] = new IRJoinV

  override val mayJoinV: WithJoin[Value] = implicitly
  override val joinRV: Join[RV] = implicitly
  override val mayJoinUnit: WithJoin[Unit] = implicitly
  override lazy val mayJoinRV: MayJoin.WithJoin[RV] = MakeJoined(using joinRV, effects)

  given Widen[Value] = new IRWidenV
  given Widen[RV] = new ProvenanceWidenRV

  override lazy val supplementaryTable: SupplementaryTable[RV] = new AbstractSupplementaryTable[RV]() {
    override def initialTable: RV = ProvenanceAbstractRelation(Seq(), Seq())
  }

  override val relationOps: ProvenanceAbstractRelationOps[Powerset[BaseIRException]] =
    new ProvenanceAbstractRelationOps(using except)

  class AnalysisAnnotator
    extends BaseAnalysisAnnotator[Value, RV, Value]
      with irarith.logger.AnalysisAnnotator[Value, RV, Value]
      with irdata.logger.AnalysisAnnotator[Value, RV, Value]
      with irstr.logger.AnalysisAnnotator[Value, RV, Value]:

    override def extractColumns(rv: RV): Seq[SupColumn] =
      relationOps.columns(rv)

    override def extractTermValue(col: SupColumn, rv: RV): Option[Value] =
      if (relationOps.hasColumn(rv, col))
        val termRV = relationOps.project(rv, Seq(col))
        assert(termRV.rows.size == 1)
        Some(termRV.rows.head)
      else
        None

  private lazy val cfgLogger = new ControlEventLogger[Value, RV](this)

  val graphBuilder: ControlEventGraphBuilder[Long, Long, BaseIRException, (FixIn, List[Any])] =
    addControlObserver(new ControlEventGraphBuilder)

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
    collected.groupBy(_._1._1).view.mapValues { m =>
      m.values.reduce((r1, r2) => Join(r1, r2).get)
    }.toMap

  val analysisAnnotator = new AnalysisAnnotator

  type Ctx = Unit

  override val fixpoint: EffectStack ?=> fix.Fixpoint[FixIn, FixOut[Value, RV]] =
    var fixPt: Combinator[FixIn, FixOut[Value, RV]] =
      fix.log(
        analysisAnnotator,
        fix.filter[FixIn, FixOut[Value, RV]]({ case _: FixIn.EnterRelation => true; case _ => false },
          fix.notContextSensitive[FixIn, FixOut[Value, RV], fix.Combinator[FixIn, FixOut[Value, RV]]](
            setLooper(fix.iter.innermost[FixIn, FixOut[Value, RV], Ctx](stackConfig))
          )
        )
      )

    if (logControlEvents)
      fixPt = fix.log(cfgLogger, fixPt)
    if (logTraversalTrace)
      fixPt = fix.log(new PrintLogger, fixPt)

    fixPt.fixpoint


class IRProvenanceAnalysis(
    val edbConfig: EdbConfig[ProvenanceAbstractRelation] = ProvenanceEdbConfig.default
  ) extends IRVisitor with Optimizer:

  val abstractInterpreter: BaseGenericInterpreter[Value, ?, ProvenanceAbstractRelation, ?, ?] =
    new IRProvenanceAbstractInterpreter(false, false, true)

  private var analysisHasRun: Boolean = false
  var idb: Map[String, ProvenanceAbstractRelation] = Map.empty

  override def analyzeProgram(modules: Seq[ir.Module]): Unit =
    if (analysisHasRun || !isClosedWorld)
      return

    analysisHasRun = true

    modules.foreach { m =>
      m.entries.foreach {
        case (_, ir.ExtensionalRelation(n, params)) =>
          val aRel = edbConfig.abstractExtensionalRelation(n, params)
          abstractInterpreter.insertEDB(n.name, aRel)
        case _ => // nothing
      }
    }

    val analysisRes = abstractInterpreter.failure.fallible {
      abstractInterpreter.evalProgram(modules)
    }

    analysisRes match
      case AFallible.Failing(failures) =>
        val msg = failures.map { (kind, message) =>
          s"[$kind]: $message"
        }.set.mkString("\n")
        throw AnalysisFailed(msg)
      case AFallible.Diverging(_) =>
        throw IllegalStateException()
      case _ => // nothing

    idb = abstractInterpreter.getIDB

  override def visitProgram(modules: Seq[ir.Module], dependencies: Seq[ir.Module]): Seq[ir.Module] =
    analyzeProgram(modules)
    super.visitProgram(modules, dependencies)
