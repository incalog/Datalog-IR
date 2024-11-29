package inca.ir.analysis

import inca.ir.analysis.base.effect.EmptySupplementary
import sturdy.data.MayJoin
import sturdy.effect.Effect

trait SupplementaryTable[RV] extends Effect:

  def scoped[A](f: => A): A

  def setTable(rv: RV): Unit

  def getTable: RV

  /** updates the supplementary table; ASSUMEs the new table is non-empty */
  def update(f: RV => RV): RV =
    val rv = f(getTable)
    setTable(rv)
    rv
