package inca.ir.extension.string.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{AType, TypeRelation, Value}
import inca.ir.extension.string.TString
import sturdy.data.MayJoin.WithJoin
import sturdy.values.{Powerset, Topped}

trait TypeAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], TypeRelation, Powerset[BaseIRException], WithJoin]:
  override val stringOps: StringOps[Value] = new StringOps[Value]:
    override def stringLit(s: String): Value = AType(TString)
    override def toString(v: Value): Value = AType(TString)
    override def concat(v1: Value, v2: Value): Value = (v1, v2) match
      case (AType(TString), AType(TString)) => AType(TString)
