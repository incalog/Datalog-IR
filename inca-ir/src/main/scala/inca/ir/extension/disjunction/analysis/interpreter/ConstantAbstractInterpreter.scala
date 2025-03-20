package inca.ir.extension.disjunction.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, AbstractRelation, Value}
import sturdy.values.Powerset
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.data.WithJoin

trait ConstantEqOps extends BaseEqOps

trait ConstantJoinV extends BaseJoinV

trait ConstantMeetV extends BaseMeetV

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]