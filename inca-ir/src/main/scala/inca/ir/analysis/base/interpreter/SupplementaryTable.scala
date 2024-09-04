package inca.ir.analysis.base.interpreter

import inca.ir.Name
import inca.ir.analysis.SupplementaryEnvironment
import inca.ir.analysis.base.values.{RelationValue, Value}
import sturdy.data.{MayJoin, WithJoin}
import sturdy.effect.Effect
import sturdy.effect.failure.Failure
import sturdy.values.{Join, Widen}


class SupplementaryTable(using j: Join[RelationValue[Name, Value]], w: Widen[RelationValue[Name, Value]], failure: Failure)
  extends SupplementaryEnvironment[RelationValue[Name, Value], WithJoin]:

  override type State = RelationValue[Name, Value]

  protected var supTable: RelationValue[Name, Value] = RelationValue(Seq(), Seq(Seq()))

  override def scoped[A](f: => A): A =
    val snapshot = supTable
    try f finally {
      supTable = snapshot
    }
  override def clear(): Unit =
    supTable = RelationValue(Seq(), Seq(Seq()))
  override def setTable(rv: RelationValue[Name, Value]): Unit =
    supTable = rv
  override def getTable: RelationValue[Name, Value] = supTable
  override def getState: RelationValue[Name, Value] = supTable

  override def setState(st: RelationValue[Name, Value]): Unit =
    this.supTable = st
  override def join: Join[RelationValue[Name, Value]] = implicitly
  override def widen: Widen[RelationValue[Name, Value]] = implicitly
    