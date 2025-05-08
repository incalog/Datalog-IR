package inca.ir.analysis

import inca.ir
import inca.ir.Type
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.{AbstractSupplementaryTable, BaseGenericInterpreter, FixIn, FixOut, SupColumn, given}
import inca.ir.analysis.base.logger.{BaseAnalysisAnnotator, PrintLogger}
import inca.ir.analysis.base.values.*
import inca.ir.extension.arithmetic.analysis as irarith
import inca.ir.extension.data.analysis as irdata
import inca.ir.extension.string.analysis as irstr
import inca.ir.extension.aggregate.analysis as iragg
import sturdy.data.MayJoin
import sturdy.data.MayJoin.WithJoin
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.effect.except.{Except, JoinedExcept}
import sturdy.effect.failure.CollectedFailures
import sturdy.fix
import sturdy.fix.HasFixpointCache
import sturdy.fix.StackConfig.StackedStates
import sturdy.values.*
import sturdy.values.booleans.{BooleanBranching, BooleanOps, ConcreteBooleanBranching, ToppedBooleanBranching, ToppedBooleanOps}
import sturdy.values.ordering.EqOps

// Implicits
import inca.ir.analysis.base.effect.{IRException, IRFailure}
import inca.ir.analysis.base.interpreter.FiniteFixIn
import inca.ir.analysis.base.values.JoinRV
import sturdy.data.{MakeJoined, given}
import sturdy.values.exceptions.PowersetExceptional
import sturdy.values.given


/**
 * The lattice used for this analysis looks like this:
 *
 *        ⊤        // <- Value.Top
 *      / | \
 *     /  |  \
 * TInt TBool ...  // <- TypeValue
 *     \  |  /
 *      \ | /
 *        ⊥         // except.throws
 *
 * - ⊤ (Top): Represents an unknown type. This is encoded with: Value.Top.
 * - TypeVale(ty: Type) represent a distinct type value. It exists only for this analysis.
 * - ⊥ (Bottom): Represents an error or uninitialized value. This is encoded as sturdy exception.
 */
case class TypeValue(ty: Type) extends Value:
  override def isConstant: Boolean = true

/**
 * Analyse the types of relations in a Datalog program.
 */
class IRTypeAbstractInterpreter(
     val enableLogging: Boolean = false,
     override val interRelational: Boolean = false
  )
  extends BaseGenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]
  with irarith.interpreter.TypeAbstractInterpreter
  with irstr.interpreter.TypeAbstractInterpreter
  with irdata.interpreter.TypeAbstractInterpreter
  with iragg.interpreter.TypeAbstractInterpreter:

  type TRV = AbstractRelation

  // Define a combine for TypeValues according to the lattice given above.
  given JoinTV: Join[Value] with {
    private def join(v1: Value, v2: Value): Value = (v2, v2) match
      case (TypeValue(ty1), TypeValue(ty2)) if ty1 == ty2 => v1
      case _ => Value.Top

    override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
      MaybeChanged(join(v1, v2), v1)
  }
  
  override lazy val topV: Value = Value.Top

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures

  override lazy val except: Except[BaseIRException, Powerset[BaseIRException], WithJoin] = new JoinedExcept(using PowersetExceptional[BaseIRException])

  override lazy val boolOps: BooleanOps[Topped[Boolean]] = ToppedBooleanOps

  given BooleanOps[Topped[Boolean]] = boolOps

  override val branchOps: BooleanBranching[Topped[Boolean], TRV] = new ToppedBooleanBranching[Boolean, TRV]

  override lazy val eqOps: EqOps[Value, Topped[Boolean]] = new EqOps[Value, Topped[Boolean]] {
    def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
      case (TypeValue(ty1), TypeValue(ty2)) => if (ty1 != ty2) Topped.Actual(false) else Topped.Top
      case _ => Topped.Top

    def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
      case (TypeValue(ty1), TypeValue(ty2)) => if (ty1 != ty2) Topped.Actual(true) else Topped.Top
      case _ => Topped.Top
  }

  given EqOps[Value, Topped[Boolean]] = eqOps
  
  override val mayJoinV: WithJoin[Value] = implicitly
  override val joinRV: Join[TRV] = implicitly
  override lazy val mayJoinRV: MayJoin.WithJoin[TRV] = MakeJoined(using joinRV, effects)

  given Widen[TRV] with {
    override def apply(v1: TRV, v2: TRV): MaybeChanged[TRV] = joinRV(v1, v2)
  }
  
  override val mayJoinUnit: WithJoin[Unit] = implicitly

  override val supplementaryTable: SupplementaryTable[TRV] = new AbstractSupplementaryTable[TRV]() {
    override def initialTable: TRV = AbstractRelation(Seq(), Seq(), Topped.Actual(false))
  }

  override val relationOps: RelationOps[Value, Topped[Boolean], TRV] = new AbstractRelationOps(using except)

  given Meet[Value] = new BaseMeetV(using except):
    override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
      case (TypeValue(t1), TypeValue(t2)) if t1 == t2 => lhs
      case _ => super.meet(lhs, rhs)

  class AnalysisAnnotator
    extends BaseAnalysisAnnotator[Value, TRV, Value]
      with irarith.logger.AnalysisAnnotator[Value, TRV, Value]
      with irdata.logger.AnalysisAnnotator[Value, TRV, Value]
      with irstr.logger.AnalysisAnnotator[Value, TRV, Value]:

    override def extractColumns(rv: TRV): Seq[String] =
      relationOps.columns(rv)
  
    override def extractTermValue(supName: SupColumn, rv: TRV): Option[Value] =
      if (relationOps.hasColumn(rv, supName))
        val termTRV = relationOps.project(rv, Seq(supName))
        assert(termTRV.rows.size == 1)
        Some(termTRV.rows.head)
      else
        None

  val analysisAnnotator: AnalysisAnnotator = new AnalysisAnnotator
  
  //fix.Fixpoint.DEBUG = true

  var looper: HasFixpointCache[FixIn, FixOut[Value, TRV]] = null
  def setLooper[A <: HasFixpointCache[FixIn, FixOut[Value, TRV]]](a: A): A =
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

  private val stackConfig = StackedStates(storeNonrecursiveOutput = true)
  override val fixpoint: EffectStack ?=> fix.Fixpoint[FixIn, FixOut[Value, TRV]] =
    val fixPt =
      fix.notContextSensitive[FixIn, FixOut[Value, TRV], fix.Combinator[FixIn, FixOut[Value, TRV]]](
        fix.filter({
          case _: FixIn.EnterRelation => true
          case _ => false // important, filter everything out we don't need
        }, setLooper(fix.iter.innermost[FixIn, FixOut[Value, TRV], Unit](stackConfig)))
        )

    val analysisFixPt = fix.log(analysisAnnotator, fixPt)

    if (enableLogging)
      fix.log(new PrintLogger, analysisFixPt).fixpoint
    else
      analysisFixPt.fixpoint
