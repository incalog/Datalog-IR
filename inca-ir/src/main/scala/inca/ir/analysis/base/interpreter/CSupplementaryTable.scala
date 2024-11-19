package inca.ir.analysis.base.interpreter

import inca.ir.Name
import inca.ir.analysis.SupplementaryEnvironment
import inca.ir.analysis.base.values.{CRelationValue, Value}
import sturdy.data.MayJoin.NoJoin
import sturdy.effect.failure.Failure
import sturdy.values.{Join, MaybeChanged, Widen, finitely}

type CRV = CRelationValue[Value]

class CSupplementaryTable(using failure: Failure, joinRV: Join[CRV])
  extends SupplementaryEnvironment[CRV, NoJoin]:

  override type State = CRV

  protected var supTable: CRV = CRelationValue(Seq(), Set(Seq()))

  override def scoped[A](f: => A): A =
    val snapshot = supTable
    try f finally {
      supTable = snapshot
    }

  override def clear(): Unit = supTable = CRelationValue(Seq(), Set(Seq()))

  def setTable(rv: CRV): Unit = setState(rv)

  def getTable: CRV = getState

  override def getState: CRV = supTable

  override def setState(st: CRV): Unit = supTable = st

  override def join: Join[CRV] = implicitly

  override def widen: Widen[CRV] = (v1: CRV, v2: CRV) => join(v1, v2)
    