package inca.ir.analysis

import inca.ir.extension.aggregate.*
import inca.ir.{Atom, Term, TermArg, WildcardArg}


trait AggregateAbstractInterpreter[V, B] extends BaseAbstractInterpreter[V, B]:

  override def evalAtomExtend(at: Atom): AtomResult = at match
    case Aggregate(rel, args, op) =>
      val vs = args.map {
        case TermArg(t) => evalTerm(t)
        case AggregateColumnArg(t) =>
          assign(t, top)
          top
        case WildcardArg() =>
          top
      }
      AtomResult(topBool, falseBool)
    case _ => super.evalAtomExtend(at)

