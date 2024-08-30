package inca.ir.analysis

import sturdy.data.MayJoin
import sturdy.effect.Effect

trait SupplementaryEnvironment[RV, J[_] <: MayJoin[_]] extends Effect:
  def scoped[A](f: => A): A
  def clear(): Unit
  def freshScoped[A](f: => A): A = scoped {
    clear()
    f
  }
  def setTable(rv: RV): Unit
  def getTable: RV
  def copy: SupplementaryEnvironment[RV, J]
