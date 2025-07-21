package inca.ir.analysis.base.logger

import inca.ir.{ Atom, Call }
import inca.ir.analysis.base.effect.BaseIRException
import sturdy.control.{BasicControlEvent, ControlEvent, ControlObservable}
import inca.ir.analysis.base.interpreter.{FixIn, FixOut}
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.fix.Logger

import java.util.Objects

trait DatalogControlObservable extends ControlObservable[Long, Long, BaseIRException, (FixIn, List[Any])]

class ControlEventLogger[V, RV](observable: DatalogControlObservable)(using effects: EffectStack) extends Logger[FixIn, FixOut[V, RV]]:
  effects.addJoinObserver(observable)

  override def enter(dom: FixIn): Unit = dom match
    case FixIn.Term(term) => // nothing
    case FixIn.Atom(call: Call) =>
      // Differentiate equal atoms with different binding information
      observable.triggerControlEvent(BasicControlEvent.BeginSection(call.id)(call.toString))
    case FixIn.Atom(atom) =>
      observable.triggerControlEvent(BasicControlEvent.Atomic(atom.id)(atom.toString))
    case FixIn.Body(rel, ix, paramNames) =>
      observable.triggerControlEvent(BasicControlEvent.BeginSection(rel.bodies(ix).id)(s"rule ${rel.name.name} at $ix"))
    case FixIn.EnterRelation(rel, adornment) =>
      observable.triggerControlEvent(BasicControlEvent.BeginSection(rel.id)(rel.name.name))
    case FixIn.Assign(_, _) =>
      // nothing, captured by atom
    case _ => throw IllegalStateException(s"Unhandled dom $dom in enter")

  override def exit(dom: FixIn, codom: TrySturdy[FixOut[V, RV]]): Unit = dom match
    case FixIn.Term(_) => // nothing
    case FixIn.Atom(call: Call) => observable.triggerControlEvent(BasicControlEvent.EndSection())
    case FixIn.Atom(_) => // nothing
    case FixIn.Body(_, _, _) => observable.triggerControlEvent(BasicControlEvent.EndSection())
    case FixIn.EnterRelation(_, _) => observable.triggerControlEvent(BasicControlEvent.EndSection())
    case FixIn.Assign(_, _) => // nothing
    case _ => throw IllegalStateException(s"Unhandled dom $dom in exit")
