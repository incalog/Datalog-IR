package inca.ir.analysis.base.interpreter

import inca.ir.Name
import inca.ir.analysis.SupplementaryTable
import inca.ir.analysis.base.values.{ARelationValue, Value}
import sturdy.data.{MayJoin, WithJoin}
import sturdy.effect.Effect
import sturdy.effect.failure.Failure
import sturdy.values.{Join, Widen}


class ASupplementaryTable(using j: Join[ARelationValue[Value]], w: Widen[ARelationValue[Value]], failure: Failure)
  extends SupplementaryTable[ARelationValue[Value]]:

  protected var supTable: ARelationValue[Value] = ARelationValue(Seq(), Some(Seq()))

  override def scoped[A](f: => A): A =
    val snapshot = supTable
    try f finally {
      supTable = snapshot
    }

  override def clear(): Unit = supTable = ARelationValue(Seq(), Some(Seq()))

  override def setTable(rv: ARelationValue[Value]): Unit = setState(rv)

  override def getTable: ARelationValue[Value] = getState

  override type State = ARelationValue[Value]
  override def getState: ARelationValue[Value] = supTable
  override def setState(st: ARelationValue[Value]): Unit = supTable = st
  override def join: Join[ARelationValue[Value]] = implicitly
  override def widen: Widen[ARelationValue[Value]] = implicitly
    