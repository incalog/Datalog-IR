package inca.ir.analysis.base.interpreter

import inca.ir.analysis.SupplementaryEnvironment
import inca.ir.analysis.base.values.{RelationValue, Value}
import sturdy.data.{MayJoin, WithJoin}
import sturdy.effect.Effect
import sturdy.effect.failure.Failure
import sturdy.values.{Join, Widen}


class SupplementaryTable(using j: Join[RelationValue], w: Widen[RelationValue], failure: Failure)
  extends SupplementaryEnvironment[RelationValue, WithJoin]:

  override type State = RelationValue

  protected var supTable: RelationValue = RelationValue(Vector(), Vector())

  override def scoped[A](f: => A): A =
    val snapshot = supTable
    try f finally {
      supTable = snapshot
    }
  override def clear(): Unit =
    supTable = RelationValue(Vector(), Vector())
  override def setTable(rv: RelationValue): Unit =
    supTable = rv
  override def getTable: RelationValue = supTable
  override def getState: RelationValue = supTable

  override def setState(st: RelationValue): Unit =
    this.supTable = st
  override def join: Join[RelationValue] = implicitly
  override def widen: Widen[RelationValue] = implicitly