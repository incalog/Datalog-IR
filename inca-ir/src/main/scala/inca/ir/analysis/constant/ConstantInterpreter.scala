package inca.ir.analysis.constant

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.analysis.base.values.{ConstantRelation, Value}
import sturdy.data.WithJoin
import sturdy.values.{Finite, Powerset, Topped}

trait ConstantInterpreter 
  extends BaseGenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]:
  
  trait ValueKind
  case object DefaultKind extends ValueKind
  given Finite[ValueKind] with {}
  
  def getValueKind(v: Value): ValueKind = DefaultKind
  
