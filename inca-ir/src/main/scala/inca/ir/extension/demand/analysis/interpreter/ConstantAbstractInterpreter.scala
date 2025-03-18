package inca.ir.extension.demand.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, ConstantRelation, Value}
import sturdy.values.Powerset
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.data.WithJoin

trait ConstantEqOps extends BaseEqOps

trait ConstantJoinV extends BaseJoinV

trait ConstantMeetV extends BaseMeetV

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]