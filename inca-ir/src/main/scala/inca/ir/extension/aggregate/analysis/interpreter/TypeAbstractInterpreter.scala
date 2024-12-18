package inca.ir.extension.aggregate.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{TypeRelation, TypeValue}
import sturdy.data.MayJoin.WithJoin
import sturdy.values.{Powerset, Topped}

trait TypeAbstractInterpreter extends GenericInterpreter[TypeValue, Topped[Boolean], TypeRelation, Powerset[BaseIRException], WithJoin]:
  ???