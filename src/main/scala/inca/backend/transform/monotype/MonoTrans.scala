package inca.backend.transform.monotype

import scala.meta._
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.{Pattern, TScalaInt}
import inca.runtime.aggregate.JoinAggregation
import inca.util.Scala

import scala.collection.mutable.ListBuffer


case class MaxAgg() extends JoinAggregation[Int] {
  override val name: String = "max"
  override def init: Int = 0
  override def join(v1: Int, v2: Int): Int = v1.max(v2)
  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
  override val hasUnjoin: Boolean = false
}



// TODO: generate instance MonoDef from mono-types Scala code directly.
// MonoDef contains the initial state, add and result method.
case class MonoDef(init: meta.Lit, add: meta.Term.Function, result: meta.Term.Function)

/** Eliminating mono-type operations in the datalog program.

  We currently only consider there is only one mono-type variable in
  the program.

 */
class MonoTrans {

  def transModule(module: Datalog.Module) : Datalog.Module = {
    Datalog.Module(
      module.name,
      module.imports,
      module.pats,
      module.scalaContent
    )
  }
}
