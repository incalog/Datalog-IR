package inca.ir.analysis.base.interpreter

import inca.ir.Name
import inca.ir.analysis.SupplementaryTable
import inca.ir.analysis.base.values.{CRelationValue, Value}
import sturdy.data.MayJoin.NoJoin
import sturdy.effect.Concrete
import sturdy.effect.failure.Failure
import sturdy.values.{Join, MaybeChanged, Widen, finitely}

type CRV = CRelationValue[Value]

class CSupplementaryTable(using failure: Failure, joinRV: Join[CRV])
  extends SupplementaryTable[CRV]://, Concrete:

  protected var supTable: CRV = CRelationValue(Seq(), Set(Seq()))

  override def scoped[A](f: => A): A =
    val snapshot = supTable
    try f finally {
      supTable = snapshot
    }

  override def clear(): Unit = supTable = CRelationValue(Seq(), Set(Seq()))

  override def setTable(rv: CRV): Unit = supTable = rv

  override def getTable: CRV = supTable

  // internal effect
  override type State = CRV
  override def getState: CRV = supTable
  override def setState(st: CRV): Unit = supTable = st
  override def join: Join[CRV] =  (v1: CRV, v2: CRV) =>
    // Is this correct?
    // The idea:
    // For Datalog the empty context {} is not the smallest possible context.
    // Evaluating a relation R(x) given an empty context produces more or equal the amount of tuples that it produces
    // under a context {x -> C}
    // This join assumes, that the context only includes variables defined as parameters of a specific relation or no
    // mappings at all
    if (v1.isUnit)
      MaybeChanged(v1, v1)
    else if (v2.isUnit)
      MaybeChanged(v2, v1)
    else
      MaybeChanged(v1.union(v2), v1) // implicitly
    /*if (v1.isEmpty)
      MaybeChanged(v1, v1)
    else if (v2.isEmpty)
      MaybeChanged(v2, v1)
    else
      MaybeChanged(v1.union(v2), v1)*/
  override def widen: Widen[CRV] = (v1: CRV, v2: CRV) => join(v1, v2)
    