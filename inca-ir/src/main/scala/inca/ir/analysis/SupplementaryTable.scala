package inca.ir.analysis

import sturdy.data.MayJoin
import sturdy.effect.Effect

trait SupplementaryTable[RV] extends Effect:

  def scoped[A](f: => A): A

  def clear(): Unit

  def freshScoped[A](f: => A): A = scoped {
    clear()
    f
  }

  def setTable(rv: RV): Unit

  def getTable: RV

  def update(f: RV => RV): Unit =
    setTable(f(getTable))
