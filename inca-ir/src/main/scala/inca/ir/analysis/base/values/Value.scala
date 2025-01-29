package inca.ir.analysis.base.values

import sturdy.values.{Finite, MaybeChanged, Join}

trait BaseJoinV extends Join[Value]:
  def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (Value.Top, _) | (_, Value.Top) => Value.Top
    case _ =>
      throw new MatchError((lhs, rhs))

  override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
    val joined = join(v1, v2)
    if v1 == joined then
      MaybeChanged.Unchanged(joined)
    else
      MaybeChanged.Changed(joined)

/*trait BaseMeetV:
  def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (Top, _) => rhs
    case (_, Top) => lhs
    case _ => Bottom*/

class FiniteV extends Finite[Value]

trait Value:
  def isConstant: Boolean

object Value:
  case object Top extends Value:
    override def isConstant: Boolean = false
