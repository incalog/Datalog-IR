package inca.ir.analysis

import inca.ir
import inca.ir.{Name, Param, Term, Type}
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.*
import inca.ir.analysis.base.logger.{BaseAnalysisAnnotator, ControlEventLogger, DatalogControlObservable, PrintLogger}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.*
import inca.ir.extension.aggregate.analysis as iragg
import inca.ir.extension.arithmetic.analysis.interpreter.{IntOps, IntervalDoubleV, IntervalIntV}
import inca.ir.extension.arithmetic.{DoubleNum, IntNum, TDouble, TInt, analysis as irarith}
import inca.ir.extension.data.analysis.interpreter.{FiniteCaseV, FiniteDataV}
import inca.ir.extension.data.{CaseDefinitionReference, DataDefinitionReference, TData, analysis as irdata}
import inca.ir.extension.string.{TString, analysis as irstr}
import inca.ir.extension.string.analysis.interpreter.FiniteStringV
import inca.ir.extension.tuple.analysis.{AbstractEdbConfig, EdbConfig}
import inca.ir.optimize.Optimizer
import inca.ir.printer.IRDebugPrinter
import inca.ir.visitors.IRVisitor
import sturdy.control.ControlEventGraphBuilder
import sturdy.data.{MayJoin, WithJoin}
import sturdy.effect.except.{Except, JoinedExcept}
import sturdy.effect.failure.{AFallible, CollectedFailures, ObservableFailure}
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.fix
import sturdy.fix.StackConfig.StackedStates
import sturdy.fix.{Combinator, Filter, HasFixpointCache, Log, StackConfig}
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.booleans.{BooleanBranching, BooleanOps, ToppedBooleanBranching, ToppedBooleanOps}
import sturdy.values.ordering.EqOps
import sturdy.values.*

// Implicits
import inca.ir.analysis.base.effect.{IRException, IRFailure}
import inca.ir.analysis.base.interpreter.{CCombineFixOut, FiniteFixIn}
import inca.ir.analysis.base.values.{FiniteJoinRV, FiniteWidenRV}
import sturdy.data.given
import sturdy.values.booleans.ConcreteBooleanBranching
import sturdy.values.exceptions.PowersetExceptional
import sturdy.values.given


/**
 * An extensible termination analysis.
 */
class IRTerminationAbstractInterpreter(
    val logTraversalTrace: Boolean = false,
    val logControlEvents: Boolean = false,
    override val interRelational: Boolean = false
  )
  extends BaseGenericInterpreter[Value, Topped[Boolean], FiniteAbstractRelation, Powerset[BaseIRException], WithJoin]
    with irarith.interpreter.IntervalAbstractInterpreter
    with irstr.interpreter.FiniteStringAbstractInterpreter
    with irdata.interpreter.FiniteAbstractInterpreter
    with iragg.interpreter.TerminationAbstractInterpreter
    with DatalogControlObservable:

  type RV = FiniteAbstractRelation

  private class IRJoinV extends Join[Value] with BaseJoinV
    with irarith.interpreter.IntervalJoinV
    with irstr.interpreter.FiniteStringJoinV
    with irdata.interpreter.FiniteJoinV:

    override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
      MaybeChanged(combine(v1, v2), v1)

  private class IRWidenV extends Widen[Value] with BaseWidenV
    with irarith.interpreter.IntervalWidenV
    with irstr.interpreter.FiniteStringWidenV
    with irdata.interpreter.FiniteDataWidenV:

    override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
      MaybeChanged(combine(v1, v2), v1)

  private class IRMeetV(using except: Except[BaseIRException, ?, ?]) extends BaseMeetV(using except)
    with irarith.interpreter.IntervalMeetV
    with irstr.interpreter.FiniteStringMeetV
    with irdata.interpreter.FiniteMeetV

  private class IREqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps
    with irarith.interpreter.IntervalEqOps
    with irstr.interpreter.FiniteStringEqOps
    with irdata.interpreter.FiniteEqOps(using boolOps)

  override lazy val topV: Value = Value.Top

  override lazy val except: Except[BaseIRException, Powerset[BaseIRException], WithJoin] = new JoinedExcept(using PowersetExceptional[BaseIRException])

  override val branchOps: BooleanBranching[Topped[Boolean], RV] = new ToppedBooleanBranching[Boolean, RV]

  val branchOpsV: BooleanBranching[Topped[Boolean], Value] = new ToppedBooleanBranching[Boolean, Value]

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures with ObservableFailure(this)

  override lazy val boolOps: BooleanOps[Topped[Boolean]] = ToppedBooleanOps

  given BooleanOps[Topped[Boolean]] = boolOps

  override lazy val eqOps: BaseEqOps = new IREqOps

  given EqOps[Value, Topped[Boolean]] = eqOps

  given Join[Value] = new IRJoinV

  override val mayJoinV: WithJoin[Value] = implicitly
  override val joinRV: Join[RV] = implicitly
  override val mayJoinUnit: WithJoin[Unit] = implicitly
  override lazy val mayJoinRV: MayJoin.WithJoin[FiniteAbstractRelation] = MakeJoined(using joinRV, effects)

  private val irWiden = new IRWidenV
  given Widen[Value] = irWiden
  given Widen[RV] = new FiniteWidenRV
  
  override lazy val supplementaryTable: SupplementaryTable[FiniteAbstractRelation] = new AbstractSupplementaryTable[RV]() {
    override def initialTable: RV = FiniteAbstractRelation(Seq(), Seq(), Topped.Actual(false), Topped.Actual(true))
  }

  given Meet[Value] = IRMeetV(using except)
  override val relationOps: FiniteAbstractRelationOps[Powerset[BaseIRException]] = new FiniteAbstractRelationOps(using except)

  class AnalysisAnnotator
    extends BaseAnalysisAnnotator[Value, RV, Value]
      with irarith.logger.AnalysisAnnotator[Value, RV, Value]
      with irdata.logger.AnalysisAnnotator[Value, RV, Value]
      with irstr.logger.AnalysisAnnotator[Value, RV, Value]
      with iragg.logger.AnalysisAnnotator[Value, RV, Value]:

    override def extractColumns(rv: RV): Seq[SupColumn] =
      relationOps.columns(rv)

    override def extractTermValue(col: SupColumn, rv: RV): Option[Value] =
      if (relationOps.hasColumn(rv, col))
        val termTRV = relationOps.project(rv, Seq(col))
        termTRV match
          case FiniteAbstractRelation.Empty(cs) => None
          case FiniteAbstractRelation.NonEmpty(cs, rows, emp, _) =>
            assert(rows.size == 1)
            Some(rows.head)
      else
        None


  override def evalModule(m: ir.Module)(using Fixed): Map[SupColumn, RV] =
    // Set up bounds for widening
    var intLits: Set[Int] = Set()
    var doubleLits: Set[Double] = Set()
    new IRVisitor {
      override def visitTerm(term: Term): Seq[Term] =
        term match
          case IntNum(value) => intLits += value
          case DoubleNum(value) => doubleLits += value
          case _ => // nothing
        super.visitTerm(term)
    }.visitModule(m)
    irWiden.intBounds = intLits
    irWiden.doubleBounds = doubleLits
    super.evalModule(m)

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


  val analysisAnnotator = new AnalysisAnnotator

  type Ctx = Unit
  override val fixpoint: EffectStack ?=> fix.Fixpoint[FixIn, FixOut[Value, RV]] =
    var fixPt: Combinator[FixIn, FixOut[Value, RV]] =
      fix.log(analysisAnnotator,
          fix.filter[FixIn, FixOut[Value, RV]]({case _: FixIn.EnterRelation => true; case _ => false},
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


case class AnalysisFailed(msg: String) extends Exception:
  override def toString: String = msg


class IRTerminationAnalysis extends IRVisitor with Optimizer:

  case class FiniteEdbConfig(data: Map[TData, Set[CaseDefinitionReference]])
    extends EdbConfig[FiniteAbstractRelation]:

    override def abstractExtensionalRelation(n: Name, params: Seq[Param]): FiniteAbstractRelation =
      def abstractEDBValueForType(ty: Type): Value = ty match
        case TInt => IntervalIntV.finite
        case TDouble => IntervalDoubleV.finite
        case TString => FiniteStringV.edb()
        case d: TData =>
          val cases = data(d)
          val abstractCases = cases.map { c =>
            val nonRecursiveArgs = c.args.filterNot(_ == d).map(abstractEDBValueForType)
            FiniteCaseV(c, nonRecursiveArgs)
          }
          FiniteDataV(abstractCases, 10)
        case _ => Value.Top

      val (aCols, aRows) = params.map(p => (p.name.name, abstractEDBValueForType(p.ty))).unzip
      FiniteAbstractRelation(aCols, aRows, Topped.Actual(false), Topped.Actual(true))

  val abstractInterpreter: BaseGenericInterpreter[Value, ?, FiniteAbstractRelation, ?, ?] =
    new IRTerminationAbstractInterpreter(false, false, true)

  private var analysisHasRun: Boolean = false

  override def analyzeProgram(modules: Seq[ir.Module]): Unit =
    if (analysisHasRun || !isClosedWorld)
      return

    println(s"Analyse now! ${modules.size}")
    println(modules.head)

    analysisHasRun = true

    var adts: Map[TData, Set[CaseDefinitionReference]] = Map()
    modules.foreach { m =>
      m.entries.foreach {
        case (_, c: CaseDefinitionReference) =>
          adts += c.data -> (adts.getOrElse(c.data, Set()) + c)
        case _ => // nothing
      }
    }

    val edbConfig = FiniteEdbConfig(adts)

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

    val idb = abstractInterpreter.getIDB
    val definitelyTerminates = idb.forall { case (_, rel) =>
      rel.finite.isActual && rel.finite.get && rel.rowsAreFinite
    }

    // TODO: Remove me after debugging
    println(new IRDebugPrinter{}.prettyPrint(modules))
    println(s"Definitely terminates: $definitelyTerminates")
    println(abstractInterpreter.getIDB)
    System.exit(1)

  override def visitProgram(modules: Seq[ir.Module], dependencies: Seq[ir.Module]): Seq[ir.Module] =
    analyzeProgram(modules)
    super.visitProgram(modules, dependencies)