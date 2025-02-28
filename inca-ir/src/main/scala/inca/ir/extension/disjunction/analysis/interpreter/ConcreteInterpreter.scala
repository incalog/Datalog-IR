package inca.ir.extension.disjunction.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{ConcreteRelation, Value}
import sturdy.data.MayJoin.NoJoin

trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], BaseIRException, NoJoin]
