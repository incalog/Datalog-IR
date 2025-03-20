package inca.ir.analysis.base.interpreter

import inca.ir.analysis.SupplementaryTable
import inca.ir.analysis.base.values.{ConcreteRelation, Value}
import sturdy.effect.failure.Failure
import sturdy.values.{Join, MaybeChanged, Widen}

type CRV = ConcreteRelation[Value]

class ConcreteSupplementaryTable(using failure: Failure, joinRV: Join[CRV])
  extends SupplementaryTable[CRV]://, Concrete:

  protected var supTable: CRV = ConcreteRelation(Seq(), Set(Seq()))

  override def scoped[A](f: => A): A =
    val snapshot = supTable
    try f finally {
      supTable = snapshot
    }

  override def setTable(rv: CRV): Unit = supTable = rv

  override def getTable: CRV = supTable

  // internal effect
  override type State = CRV
  override def getState: CRV = supTable
  override def setState(st: CRV): Unit = supTable = st
  override def join: Join[CRV] = (v1: CRV, v2: CRV) => MaybeChanged(v1.join(v2), v1)
  override def widen: Widen[CRV] = (v1: CRV, v2: CRV) => join(v1, v2)
    