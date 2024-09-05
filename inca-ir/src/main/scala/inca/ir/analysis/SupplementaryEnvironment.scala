package inca.ir.analysis

import sturdy.data.MayJoin
import sturdy.effect.Effect

trait SupplementaryEnvironment[RV, J[_] <: MayJoin[_]] extends Effect:
  override type State = RV

  def scoped[A](f: => A): A
  def clear(): Unit
  def freshScoped[A](f: => A): A = scoped {
    clear()
    f
  }