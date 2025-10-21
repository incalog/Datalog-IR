package inca.ir.analysis

import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.{Name, Param}
import sturdy.values.Topped

trait EdbConfig[RV]:
  def abstractExtensionalRelation(n: Name, params: Seq[Param]): RV

class AbstractEdbConfig extends EdbConfig[AbstractRelation]:
  override def abstractExtensionalRelation(n: Name, params: Seq[Param]): AbstractRelation =
    val (aCols, aRows) = params.map(p => (p.name.name, Value.Top)).unzip
    AbstractRelation(aCols, aRows, Topped.Actual(false)) // assume non-empty edb

object AbstractEdbConfig:
  val default: AbstractEdbConfig = new AbstractEdbConfig
