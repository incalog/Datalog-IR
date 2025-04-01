package inca.ir.extension.demand.analysis.interpreter

import inca.ir.Relation
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{AbstractRelation, BaseJoinV, BaseMeetV, Value}
import inca.ir.extension.demand.TDemand
import sturdy.values.Powerset
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.data.WithJoin
import inca.ir

trait ConstantEqOps extends BaseEqOps

trait ConstantJoinV extends BaseJoinV

trait ConstantMeetV extends BaseMeetV

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  def provideValueForDemandedParam(param: ir.Param): Value = Value.Top