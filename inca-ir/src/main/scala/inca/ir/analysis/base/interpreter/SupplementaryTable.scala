package inca.ir.analysis.base.interpreter

import inca.ir.Name
import inca.ir.analysis.SupplementaryEnvironment
import inca.ir.analysis.base.values.{RelationValue, Value}
import sturdy.data.{MayJoin, WithJoin}
import sturdy.effect.Effect
import sturdy.effect.failure.Failure
import sturdy.values.{Join, Widen}


class SupplementaryTable(using j: Join[RelationValue[Value]], w: Widen[RelationValue[Value]], failure: Failure)
  extends SupplementaryEnvironment[RelationValue[Value], WithJoin]:

  override type State = RelationValue[Value]

  protected var supTable: RelationValue[Value] = RelationValue(Seq(), Some(Seq()))

  override def scoped[A](f: => A): A =
    val snapshot = supTable
    try f finally {
      supTable = snapshot
    }

  override def clear(): Unit = supTable = RelationValue(Seq(), Some(Seq()))

  def setTable(rv: RelationValue[Value]): Unit = setState(rv)

  def getTable: RelationValue[Value] = getState

  override def getState: RelationValue[Value] = supTable

  override def setState(st: RelationValue[Value]): Unit = supTable = st

  override def join: Join[RelationValue[Value]] = implicitly

  override def widen: Widen[RelationValue[Value]] = implicitly
    