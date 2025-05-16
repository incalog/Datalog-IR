package inca.ir.extension.string.analysis.interpreter

import inca.ir.analysis.TypeValue
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.string.TString
import sturdy.data.MayJoin.WithJoin
import sturdy.values.{Powerset, Topped}

trait TypeAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  lazy val stringOps: StringOps[Topped[Boolean], Value] = new StringOps[Topped[Boolean], Value]:
    override def stringLit(s: String): Value = TypeValue(TString)
    override def toString(v: Value): Value = TypeValue(TString)
    override def concat(v1: Value, v2: Value): Value = (v1, v2) match
      case (TypeValue(TString), TypeValue(TString)) => TypeValue(TString)
    override def substring(v: Value, index: Value, length: Value): Value = TypeValue(TString)
    override def stringLength(v: Value): Value = TypeValue(TInt)
    override def ordinalNumber(v: Value): Value = TypeValue(TInt)
    override def matches(v: Value, pattern: Value): Topped[Boolean] = Topped.Top
    override def stringValue(v: Value): String =
      throw IllegalStateException(s"Can not get string value of type value $v")
