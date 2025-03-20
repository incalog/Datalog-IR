package inca.ir.analysis.base.interpreter

import inca.ir.analysis.SupplementaryTable
import inca.ir.analysis.base.values.Value
import sturdy.effect.failure.Failure
import sturdy.values.{Join, Widen}


trait AbstractSupplementaryTable[RV](using j: Join[RV], w: Widen[RV], failure: Failure)
  extends SupplementaryTable[RV]:

  protected var supTable: RV = initialTable

  def initialTable: RV

  override def scoped[A](f: => A): A =
    val snapshot = supTable
    try f finally {
      supTable = snapshot
    }

  override def setTable(rv: RV): Unit = setState(rv)

  override def getTable: RV = getState

  override type State = RV
  override def getState: RV = supTable
  override def setState(st: RV): Unit = supTable = st
  override def join: Join[RV] = implicitly
  override def widen: Widen[RV] = implicitly
    