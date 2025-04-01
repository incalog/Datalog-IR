package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, AbstractRelation, Value}
import inca.ir.extension.data.CaseDefinitionReference
import inca.ir.extension.string
import sturdy.values.Powerset
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.data.WithJoin

trait ClassOps[V, B]:
  def isSubclass(v1: V, v2: V): B // should be reflexive
  def join(v1: V, v2: V): V
  def meet(v1: V, v2: V): V

case class OODLClassV(clsName: String) extends Value:
  override def toString: String = s"$$$clsName"
  override def isConstant: Boolean = false

object OODLClassV:
  val Base: OODLClassV = OODLClassV("Object")
  val Null: OODLClassV = OODLClassV("Null")

trait OODLClassEqOps(using boolOps: BooleanOps[Topped[Boolean]], classOps: ClassOps[OODLClassV, Boolean]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (cls1: OODLClassV, cls2: OODLClassV) if classOps.isSubclass(cls1, cls2) => Topped.Top
    case (cls1: OODLClassV, cls2: OODLClassV) if classOps.isSubclass(cls2, cls1) => Topped.Top
    case (cls1: OODLClassV, cls2: OODLClassV) => Topped.Actual(false)
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (cls1: OODLClassV, cls2: OODLClassV) if classOps.isSubclass(cls1, cls2) => Topped.Top
    case (cls1: OODLClassV, cls2: OODLClassV) if classOps.isSubclass(cls2, cls1) => Topped.Top
    case (cls1: OODLClassV, cls2: OODLClassV) => Topped.Actual(true)
    case _ => super.neq(v1, v2)

trait OODLClassJoinV(using classOps: ClassOps[OODLClassV, Boolean]) extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (cls1: OODLClassV, cls2: OODLClassV) => classOps.join(cls1, cls2)
    case _ => super.join(lhs, rhs)

trait OODLClassMeetV(using classOps: ClassOps[OODLClassV, Boolean]) extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (cls1: OODLClassV, cls2: OODLClassV) => classOps.meet(cls1, cls2)
    case _ => super.meet(lhs, rhs)

trait OODLClassAbstractInterpreter
  extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]
  with string.analysis.interpreter.GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:

  val classOps: ClassOps[OODLClassV, Boolean]

  val dataOps: DataOps[Value, AbstractRelation] = new DataOps[Value, AbstractRelation]:
    override def construct(caseDef: CaseDefinitionReference, args: Seq[Value]): Value =
      (caseDef.data.ref.name.name, args.headOption) match
        case ("ID", Some(Value.Top)) => OODLClassV.Base
        case ("ID", Some(v)) => OODLClassV(stringOps.stringValue(v))
        case _ => Value.Top

    override def deconstruct(v: Value, caseDef: CaseDefinitionReference)(matching: Seq[Value] => AbstractRelation)(notMatching: => AbstractRelation): AbstractRelation = v match
      case OODLClassV(cls) if caseDef.data.ref.name.name == "ID" =>
        // If it matches, the first argument is the exact class
        effects.joinComputations {
          matching(stringOps.stringLit(cls) +: caseDef.args.tail.map(_ => Value.Top))
        } {
          notMatching
        }
      case Value.Top =>
        // Could or could not match
        effects.joinComputations {
          matching(caseDef.args.map(_ => Value.Top))
        } {
          notMatching
        }
