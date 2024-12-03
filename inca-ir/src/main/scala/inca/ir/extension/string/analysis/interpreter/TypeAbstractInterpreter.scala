package inca.ir.extension.string.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{TypeRelation, TypeValue}
import inca.ir.extension.string.TString
import sturdy.data.MayJoin.WithJoin
import sturdy.values.{Powerset, Topped}

trait TypeAbstractInterpreter extends GenericInterpreter[TypeValue, Topped[Boolean], TypeRelation, Powerset[BaseIRException], WithJoin]:
  override val stringOps: StringOps[TypeValue] = new StringOps[TypeValue]:
    override def stringLit(s: String): TypeValue = TypeValue.AType(TString)
    override def toString(v: TypeValue): TypeValue = TypeValue.AType(TString)
    override def concat(v1: TypeValue, v2: TypeValue): TypeValue = (v1, v2) match
      case (TypeValue.AType(TString), TypeValue.AType(TString)) => TypeValue.AType(TString)
