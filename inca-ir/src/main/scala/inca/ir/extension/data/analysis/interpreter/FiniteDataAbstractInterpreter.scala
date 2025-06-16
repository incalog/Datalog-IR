package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, BaseWidenV, FiniteAbstractRelation, Value}
import inca.ir.extension.data.{CaseDefinitionReference, DataDefinitionReference}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.values.floating.{FloatOps, LiftedFloatOps, ToppedFloatOps, given}
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.integer.{ConcreteIntegerOps, IntegerOps, LiftedIntegerOps, ToppedIntegerOps}
import sturdy.values.ordering.{LiftedOrderingOps, OrderingOps, ToppedCertainOrderingOps}
import sturdy.data.{MakeJoined, WithJoin}
import sturdy.values.integer.given_OrderingOps_Int_Boolean

case class FiniteCaseV(caseDef: CaseDefinitionReference, args: Seq[Value]):
  override def toString: String = s"${caseDef.name}${args.mkString("(", ",", ")")}"
  def isFinite: Boolean = args.forall(_.isFinite)

case class FiniteDataV(alternatives: Set[FiniteCaseV], depth: Int) extends Value:
  private lazy val alternativesMap: Map[CaseDefinitionReference, Seq[Value]] =
    alternatives.map(c => c.caseDef -> c.args).toMap

  def caseDefs: Set[CaseDefinitionReference] =
    alternatives.map(_.caseDef)

  def nonRecursiveArgsForCase(caseDef: CaseDefinitionReference): Option[Seq[Value]] =
    alternativesMap.get(caseDef)

  def argsForCase(caseDef: CaseDefinitionReference): Seq[Value] =
    // Filter out recursive arguments
    if (!alternativesMap.contains(caseDef))
      throw IllegalStateException("Case is not contained")
    val numArgs = caseDef.args.size
    // TODO: Do we need to handle indirect recursion?
    val dataDef = caseDef.data
    val recursiveIndices: Seq[Int] = caseDef.args
      .zipWithIndex
      .filter(_._1 == dataDef)
      .map(_._2)
    var nonRecursiveArgs = nonRecursiveArgsForCase(caseDef).get
    for (i <- 0.until(numArgs)) yield {
      if (recursiveIndices.contains(i))
        FiniteDataV(alternatives, depth - 1)
      else
        val (h, t) = (nonRecursiveArgs.head, nonRecursiveArgs.tail)
        nonRecursiveArgs = t
        h
    }

  override def isConstant: Boolean = false
  override def isFinite: Boolean = alternatives.forall(_.isFinite)
  override def toString: String = s"Data({${alternatives.mkString(",")}} | $depth)"

object FiniteDataV:
  def construct(caseDef: CaseDefinitionReference, args: Seq[Value]): FiniteDataV =
    // TODO: Do we need to handle indirect recursion?
    def isRecursive(arg: Value): Boolean = arg match
      case v: FiniteDataV => v.caseDefs.map(_.data).contains(caseDef.data)
      case _ => false

    val innerDepths = args.flatMap{
      case FiniteDataV(_, d) => Some(d)
      case _ => None
    }
    val argDepth =
      if (innerDepths.isEmpty) 0
      else innerDepths.max

    new FiniteDataV(Set(FiniteCaseV(caseDef, args.filterNot(isRecursive))), argDepth + 1)


trait FiniteEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (FiniteDataV(alts1, _), FiniteDataV(alts2, _)) =>
      if (alts1.intersect(alts2).isEmpty)
        Topped.Actual(false)
      else
        Topped.Top
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (FiniteDataV(alts1, _), FiniteDataV(alts2, _)) =>
      if (alts1.intersect(alts2).isEmpty)
        Topped.Actual(true)
      else
        Topped.Top
    case _ => super.neq(v1, v2)

trait FiniteJoinV extends BaseJoinV:
  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (i1@FiniteDataV(alts1, d1), i2@FiniteDataV(alts2, d2)) =>
      //if (d1 != d2)
      //  println(s"Join: $d1 :: $d2")
      val newDepth = d1.max(d2)
      val caseDefs = i1.caseDefs ++ i2.caseDefs
      val alts = caseDefs.map { c =>
        val vs = (i1.nonRecursiveArgsForCase(c), i2.nonRecursiveArgsForCase(c)) match
          case (None, Some(vs2)) => vs2
          case (Some(vs1), None) => vs1
          case (Some(vs1), Some(vs2)) => vs1.zip(vs2).map(combine(_, _))
          case (None, None) => throw IllegalStateException("Not possible")
        FiniteCaseV(c, vs)
      }
      FiniteDataV(alts, newDepth)
    case _ => super.combine(lhs, rhs)

trait FiniteDataWidenV extends BaseWidenV:
  var maxDepth: Int = 20

  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (i1@FiniteDataV(alts1, d1), i2@FiniteDataV(alts2, d2)) =>
      //if (d1 != d2)
      //  println(s"Widen: $d1 :: $d2")
      val newDepth = d1.max(d2)
      if (newDepth > maxDepth)
        Value.Top
      else
        val caseDefs = i1.caseDefs ++ i2.caseDefs
        val alts = caseDefs.map { c =>
          val vs = (i1.nonRecursiveArgsForCase(c), i2.nonRecursiveArgsForCase(c)) match
            case (None, Some(vs2)) => vs2
            case (Some(vs1), None) => vs1
            case (Some(vs1), Some(vs2)) => vs1.zip(vs2).map(combine(_, _))
            case (None, None) => throw IllegalStateException("Not possible")
          FiniteCaseV(c, vs)
        }
        FiniteDataV(alts, newDepth)
    case _ => super.combine(lhs, rhs)

trait FiniteMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (i1@FiniteDataV(alts1, d1), i2@FiniteDataV(alts2, d2)) =>
      val newDepth = d1.min(d2)
      val caseDefs = i1.caseDefs.intersect(i2.caseDefs)
      val alts = caseDefs.map { c =>
        val vs = (i1.nonRecursiveArgsForCase(c), i2.nonRecursiveArgsForCase(c)) match
          case (None, Some(vs2)) => vs2
          case (Some(vs1), None) => vs1
          case (Some(vs1), Some(vs2)) => vs1.zip(vs2).map(meet(_, _))
          case (None, None) => throw IllegalStateException("Not possible")
        FiniteCaseV(c, vs)
      }
      FiniteDataV(alts, newDepth)
    case _ => super.meet(lhs, rhs)

trait FiniteAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], FiniteAbstractRelation, Powerset[BaseIRException], WithJoin]:

  val dataOps: DataOps[Value, FiniteAbstractRelation] = new DataOps[Value, FiniteAbstractRelation]:
    override def construct(caseDef: CaseDefinitionReference, args: Seq[Value]): Value =
      FiniteDataV.construct(caseDef, args)

    override def deconstruct(v: Value, caseDef: CaseDefinitionReference)(matching: Seq[Value] => FiniteAbstractRelation)(notMatching: => FiniteAbstractRelation): FiniteAbstractRelation = v match
      case i1: FiniteDataV if i1.caseDefs.contains(caseDef) =>
        matching(i1.argsForCase(caseDef))
      case i1: FiniteDataV =>
        notMatching
      case Value.Top =>
        // Could or could not match
        effects.joinComputations {
          matching(caseDef.args.map(_ => Value.Top))
        } {
          notMatching
        }

    override def deconstructNeg(v: Value, caseDef: CaseDefinitionReference)(possibleSuccess: Seq[Value] => FiniteAbstractRelation)(success: => FiniteAbstractRelation): FiniteAbstractRelation = v match
      case i1: FiniteDataV if i1.caseDefs.contains(caseDef) =>
        possibleSuccess(i1.argsForCase(caseDef))
      case _: FiniteDataV =>
        success
      case Value.Top =>
        // Could or could not match
        effects.joinComputations {
          possibleSuccess(caseDef.args.map(_ => Value.Top))
        } {
          success
        }