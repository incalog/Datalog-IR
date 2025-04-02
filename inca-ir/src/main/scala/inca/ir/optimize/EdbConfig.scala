package inca.ir.optimize

import inca.ir.{Name, Param}
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.extension.arithmetic.analysis.interpreter.ConstantIntV
import sturdy.values.Topped

trait EdbConfig[RV]:
  def abstractExtensionalRelation(n: Name, params: Seq[Param]): RV

class AbstractEdbConfig extends EdbConfig[AbstractRelation]:
  override def abstractExtensionalRelation(n: Name, params: Seq[Param]): AbstractRelation =
    val (aCols, aRows) = params.map(p => (p.name.name, Value.Top)).unzip
    AbstractRelation(aCols, aRows, Topped.Actual(false)) // assume non-empty edb

object AbstractEdbConfig:
  val default: AbstractEdbConfig = new AbstractEdbConfig

class OODLEdbConfig extends AbstractEdbConfig:
  override def abstractExtensionalRelation(n: Name, params: Seq[Param]): AbstractRelation =
    if (n.name == "ext_main$input")
      // OODL specific
      val (aCols, aRows) = params.map {
        case p if p.name.name == "Alloc" => (p.name.name, ConstantIntV(1))
        case p if p.name.name == "Mutation" => (p.name.name, ConstantIntV(1))
        case p if p.name.name == "MonoImpurity" => (p.name.name, ConstantIntV(1))
        case p => (p.name.name, Value.Top)
      }.unzip
      AbstractRelation(aCols, aRows, Topped.Actual(false))
    else
      super.abstractExtensionalRelation(n, params)

object OODLEdbConfig:
  val default: OODLEdbConfig = new OODLEdbConfig
