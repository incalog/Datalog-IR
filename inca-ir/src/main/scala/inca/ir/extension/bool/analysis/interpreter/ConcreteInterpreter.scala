package inca.ir.extension.bool.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{ConcreteRelation, Value}
import sturdy.values.Powerset
import sturdy.data.MayJoin.NoJoin
import sturdy.values.booleans.BooleanOps

case class CBoolV(bool: Boolean) extends Value:
  override def toString: String = bool.toString
  override def isConstant: Boolean = true

trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], Powerset[BaseIRException], NoJoin]:
  val booleanOps: BooleanOps[Value] = new BooleanOps[Value]:

    override def boolLit(b: Boolean): Value = CBoolV(b)

    override def and(v1: Value, v2: Value): Value = (v1, v2) match
      case (_, CBoolV(false)) | (CBoolV(false), _) => CBoolV(false)
      case (CBoolV(b1), CBoolV(b2)) => CBoolV(b1 && b2)
      case _ => failure(InvalidBooleanOp, s"Can not apply logical and between $v1 and $v2")

    override def or(v1: Value, v2: Value): Value = (v1, v2) match
      case (_, CBoolV(true)) | (CBoolV(true), _) => CBoolV(true)
      case (CBoolV(b1), CBoolV(b2)) => CBoolV(b1 || b2)
      case _ => failure(InvalidBooleanOp, s"Can not apply logical or between $v1 and $v2")

    override def not(v: Value): Value = v match
      case CBoolV(b) => CBoolV(!b)
      case _ => failure(InvalidBooleanOp, s"Can not negate $v")