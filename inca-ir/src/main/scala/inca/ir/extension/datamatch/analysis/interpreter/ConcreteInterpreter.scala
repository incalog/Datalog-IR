package inca.ir.extension.datamatch.analysis.interpreter

import inca.ir.analysis.base.effect.{BaseIRException, BaseIRFailure}
import inca.ir.analysis.base.values.{ConcreteRelation, Value}
import sturdy.values.Powerset
import sturdy.data.MayJoin.NoJoin


trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], BaseIRException, NoJoin]