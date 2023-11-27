package inca.ir.analysis

import inca.ir.extension.data.*
import inca.ir.{Atom, Term, TermArg}



trait DataAbstractInterpreter[V, B] extends BaseAbstractInterpreter[V, B]:

  trait DataOps[V]:
    def construct(name: String, args: Seq[V]): V
    def deconstruct(v: V, name: String, fail: () => AtomResult)(success: Seq[V] => AtomResult): AtomResult

  val dataOps: DataOps[V]

  override def evalAtomExtend(at: Atom): AtomResult = at match
    case NegDeconstruct(t, caseName) =>
      val TermResult(v, p) = evalTerm(t)
      dataOps.deconstruct(v, caseName.name, () => AtomResult(trueBool, trueBool))(_ => AtomResult(falseBool, trueBool))
    case Deconstruct(t, caseName, pats) =>
      val TermResult(v, p) = evalTerm(t)
      dataOps.deconstruct(v, caseName.name, () => AtomResult(falseBool, trueBool)) { vs =>
        val as = pats.zip(vs).map {
          case (TermArg(t), v) => assign(t, v)
          case _ => AtomResult(trueBool, trueBool)
        }
        as.foldLeft(AtomResult(trueBool, trueBool))(_&&_)
      }
    case _ => super.evalAtomExtend(at)

  override def evalTermExtend(term: Term): TermResult = term match
    case Construct(cons, args) =>
      val vs = args.map(evalTermExtend)
      val res = dataOps.construct(cons.name, vs.map(_.value))
      val pure = vs.foldLeft(trueBool)((b, v) => boolOps.and(b, v.pure))
      TermResult(res, pure)
    case _ => super.evalTermExtend(term)