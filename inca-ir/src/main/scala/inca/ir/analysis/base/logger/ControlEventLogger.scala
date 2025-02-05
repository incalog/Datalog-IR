package inca.ir.analysis.base.logger

import inca.ir.{ Atom, Call }
import inca.ir.analysis.base.effect.BaseIRException
import sturdy.control.{BasicControlEvent, ControlEvent, ControlObservable}
import inca.ir.analysis.base.interpreter.{FixIn, FixOut}
import sturdy.effect.{EffectStack, TrySturdy}
import sturdy.fix.Logger

import java.util.Objects

trait DatalogControlObservable extends ControlObservable[Int, String, BaseIRException, (FixIn, List[Any])]

class ControlEventLogger[V, RV](observable: DatalogControlObservable)(using effects: EffectStack) extends Logger[FixIn, FixOut[V, RV]]:
  effects.addJoinObserver(observable)

  override def enter(dom: FixIn): Unit = dom match
    case FixIn.Term(term) => // nothing
    case FixIn.Atom(call: Call, body) =>
      // Differentiate equal atoms with different binding information
      val hash = Objects.hash(call +: body +: call.vars.map(_.typ):_*)
      observable.triggerControlEvent(BasicControlEvent.BeginSection(hash.toString)(call.toString))
    case FixIn.Atom(atom, body) =>
      val hash = Objects.hash(atom +: body +: atom.vars.map(_.typ):_*)
      observable.triggerControlEvent(BasicControlEvent.Atomic(hash)(atom.toString))
    case FixIn.Body(rel, ix, paramNames) =>
      observable.triggerControlEvent(BasicControlEvent.BeginSection(s"rule ${rel.name} $ix")(""))
    case FixIn.EnterRelation(rel, adornment) =>
      observable.triggerControlEvent(BasicControlEvent.BeginSection(rel.name.name)(""))
    case FixIn.Assign(_, _) =>
      // nothing, captured by atom

  override def exit(dom: FixIn, codom: TrySturdy[FixOut[V, RV]]): Unit = dom match
    case FixIn.Term(_) => // nothing
    case FixIn.Atom(call: Call, _) => observable.triggerControlEvent(BasicControlEvent.EndSection())
    case FixIn.Atom(_, _) =>
    case FixIn.Body(_, _, _) => observable.triggerControlEvent(BasicControlEvent.EndSection())
    case FixIn.EnterRelation(_, _) => observable.triggerControlEvent(BasicControlEvent.EndSection())
    case FixIn.Assign(_, _) => // nothing
