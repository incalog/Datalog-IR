package inca.ir.analysis

import inca.ir.extension.aggregate.*
import inca.ir.{Atom, Term}


trait AggregateAbstractInterpreter[V, B] extends BaseAbstractInterpreter[V, B]:

  override def evalAtomExtend(at: Atom): AtomResult = at match
    case Aggregate(rel, args, op) =>
      val vs = args.map {
        case AggregateArg.Arg(t) => evalTerm(t)
        case AggregateArg.AggregateColumn(t) =>
          assign(t, top)
          top
        case AggregateArg.WildCard(t) =>
          assign(t, top)
          top
      }
      AtomResult(topBool, falseBool)
    case _ => super.evalAtomExtend(at)

