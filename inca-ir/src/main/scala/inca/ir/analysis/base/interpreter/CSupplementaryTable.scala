package inca.ir.analysis.base.interpreter

import inca.ir.Name
import inca.ir.analysis.SupplementaryEnvironment
import inca.ir.analysis.base.values.{CRelationValue, Value}
import sturdy.data.MayJoin.NoJoin
import sturdy.effect.failure.Failure
import sturdy.values.{Join, Widen}


class CSupplementaryTable(using failure: Failure)
  extends SupplementaryEnvironment[CRelationValue[Value], NoJoin]:

  override type State = CRelationValue[Value]

  protected var supTable: CRelationValue[Value] = CRelationValue(Seq(), Set(Seq()))

  override def scoped[A](f: => A): A =
    val snapshot = supTable
    try f finally {
      supTable = snapshot
    }

  override def clear(): Unit = supTable = CRelationValue(Seq(), Set(Seq()))

  def setTable(rv: CRelationValue[Value]): Unit = setState(rv)

  def getTable: CRelationValue[Value] = getState

  override def getState: CRelationValue[Value] = supTable

  override def setState(st: CRelationValue[Value]): Unit = supTable = st

  // What should these do here?
  override def join: Join[CRelationValue[Value]] = throw IllegalStateException("Join not possible")

  override def widen: Widen[CRelationValue[Value]] = throw IllegalStateException("Widen not possible")
    