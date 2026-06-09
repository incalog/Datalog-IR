package inca.ir.extension.string.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{ProvenanceAbstractRelation, ProvenanceV, Value}
import sturdy.data.WithJoin
import sturdy.values.{Powerset, Topped}


private val RuntimeStringV: Value = ProvenanceV("runtime.string")
private val RuntimeIntV: Value = ProvenanceV("runtime.int")

class ProvenanceStringOps extends StringOps[Topped[Boolean], Value]:
  override def stringLit(s: String): Value = RuntimeStringV
  override def toString(v: Value): Value = RuntimeStringV
  override def concat(v1: Value, v2: Value): Value = RuntimeStringV
  override def substring(v: Value, index: Value, length: Value): Value = RuntimeStringV
  override def stringLength(v: Value): Value = RuntimeIntV
  override def ordinalNumber(v: Value): Value = RuntimeIntV
  override def matches(v: Value, pattern: Value): Topped[Boolean] = Topped.Top
  override def stringValue(v: Value): String = v.toString


trait ProvenanceAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ProvenanceAbstractRelation, Powerset[BaseIRException], WithJoin]:
  override lazy val stringOps: StringOps[Topped[Boolean], Value] = ProvenanceStringOps()
