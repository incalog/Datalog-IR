package inca.ir.extension.string.analysis.interpreter

import inca.ir.analysis.TypeValue
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.extension.string.TString
import sturdy.data.MayJoin.WithJoin
import sturdy.values.{Powerset, Topped}

trait TypeAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  override val stringOps: StringOps[Value] = new StringOps[Value]:
    override def stringLit(s: String): Value = TypeValue(TString)
    override def toString(v: Value): Value = TypeValue(TString)
    override def concat(v1: Value, v2: Value): Value = (v1, v2) match
      case (TypeValue(TString), TypeValue(TString)) => TypeValue(TString)
