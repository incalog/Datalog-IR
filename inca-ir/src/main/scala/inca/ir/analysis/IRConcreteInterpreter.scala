package inca.ir.analysis

import inca.ir
import inca.ir.analysis.base.effect
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, CSupplementaryTable, FixIn, FixOut, given}
import inca.ir.analysis.base.logger.PrintLogger
import inca.ir.analysis.base.ordering.BaseAtomOrderingOps
import inca.ir.analysis.base.values.*
import inca.ir.extension.arithmetic.analysis as irarith
import inca.ir.extension.string.analysis as irstr
import inca.ir.extension.data.analysis as irdata
import inca.ir.extension.aggregate.analysis as iragg
import inca.ir.extension.tuple.analysis as irtuple
import inca.ir.extension.bool.analysis as irbool
import sturdy.data.MayJoin
import sturdy.data.MayJoin.{NoJoin, WithJoin}
import sturdy.effect.except.{ConcreteExcept, Except, JoinedExcept}
import sturdy.effect.failure.CollectedFailures
import sturdy.effect.{Concrete, EffectStack, TrySturdy}
import sturdy.fix
import sturdy.fix.{HasFixpointCache, StackConfig}
import sturdy.fix.StackConfig.StackedStates
import sturdy.values.booleans.{BooleanBranching, BooleanOps, ConcreteBooleanBranching, ConcreteBooleanOps}
import sturdy.values.ordering.EqOps
import sturdy.values.*

// Implicits
import sturdy.data.given
import sturdy.values.given
import inca.ir.analysis.base.effect.IRFailure
import inca.ir.analysis.base.effect.IRException
import inca.ir.analysis.base.interpreter.FiniteFixIn
import inca.ir.analysis.base.values.JoinCRV
import sturdy.values.exceptions.ConcreteExceptional

class IRConcreteInterpreter(val enableLogging: Boolean = false)
  extends BaseGenericInterpreter[Value, Boolean, ConcreteRelation[Value], BaseIRException, NoJoin]
  with irarith.interpreter.ConcreteInterpreter
  with irstr.interpreter.ConcreteInterpreter
  with irdata.interpreter.ConcreteInterpreter
  with iragg.interpreter.ConcreteInterpreter
  with irtuple.interpreter.ConcreteInterpreter
  with irbool.interpreter.ConcreteInterpreter:

  type CRV = ConcreteRelation[Value]

  private class IRAtomOrderingOps extends BaseAtomOrderingOps
    with irdata.ordering.AtomOrderingOps
  
  override val atomOrderingOps = new IRAtomOrderingOps
  
  // Concrete interpretation must always be inter-relational
  override val interRelational: Boolean = true

  override lazy val topV: Value = throw IllegalStateException("Concrete concrete does not support top value!")

  // We don't join excepts in the concrete interpreter, because there is no case where we would need to join
  override lazy val mayJoinRV: MayJoin.NoJoin[ConcreteRelation[Value]] = noJoin

  override lazy val failure: CollectedFailures[effect.BaseIRFailure] = new CollectedFailures

  override lazy val boolOps: BooleanOps[Boolean] = ConcreteBooleanOps

  override lazy val except: Except[BaseIRException, BaseIRException, NoJoin] = new ConcreteExcept(using ConcreteExceptional[BaseIRException]) //new JoinedExcept(using PowersetExceptional[BaseIRException])

  given BooleanOps[Boolean] = boolOps

  override val branchOps: BooleanBranching[Boolean, CRV] = ConcreteBooleanBranching


  override lazy val eqOps: EqOps[Value, Boolean] = new EqOps[Value, Boolean] {
    def equ(v1: Value, v2: Value): Boolean = v1 == v2
    def neq(v1: Value, v2: Value): Boolean = v1 != v2
  }
  
  override val joinV: NoJoin[Value] = implicitly
  override val joinRV: Join[CRV] = implicitly
  
  // Only correct for concrete Datalog concrete
  given Widen[CRV] with {
    override def apply(v1: CRV, v2: CRV): MaybeChanged[CRV] = joinRV(v1, v2)
  }
  
  override val joinUnit: NoJoin[Unit] = implicitly

  override val supplementaryTable: CSupplementaryTable = new CSupplementaryTable

  given EqOps[Value, Boolean] = eqOps

  override val relationOps: RelationOps[Value, Boolean, CRV] = new ConcreteRelationOps[Value]


  var looper: HasFixpointCache[FixIn, FixOut[Value, CRV]] = null
  def setLooper[A <: HasFixpointCache[FixIn, FixOut[Value, CRV]]](a: A): A =
    looper = a
    a
  override def getIDB: Map[String, ConcreteRelation[Value]] =
    val collected = looper.getCache.collect {
      case (FixIn.EnterRelation(rel, adorn), TrySturdy.Success(FixOut.Relation(rv))) => (rel.name.name, adorn) -> rv
    }
    val reduced = collected.groupBy(_._1._1).view.mapValues { m =>
      m.values.reduce((r1,r2) => Join(r1,r2).get)
    }.toMap
    reduced

  val stackConfig: StackConfig = StackedStates(storeNonrecursiveOutput = true)

  override val fixpoint: EffectStack ?=> fix.Fixpoint[FixIn, FixOut[Value, CRV]] =
    val fixPt =
      fix.notContextSensitive[FixIn, FixOut[Value, CRV], fix.Combinator[FixIn, FixOut[Value, CRV]]](
        fix.filter(_.isInstanceOf[FixIn.EnterRelation], {
          setLooper(fix.iter.innermost[FixIn, FixOut[Value, CRV], Unit](stackConfig))
        })
      )

    if (enableLogging)
      fix.log(new PrintLogger, fixPt).fixpoint
    else
      fixPt.fixpoint



