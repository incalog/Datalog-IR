package inca.ir.extension.demand

import inca.ir.Hint.preserveHints
import inca.ir.{Atom, Body, Name, Param, TNothing, Term, Type}
import inca.ir.extension.disjunction.Disjunction
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation

/*
R(y) :- {A1(x) or A2(x)}, Q(x, y).
Q(x, y) :- demand(x), B(x, y), C(x, y).

R(y) :- {A1(x) or A2(x)}, B(x, y), C(x, y).

R(y) :- {A1(x) or A2(x)}, demand(x), B(x, y), C(x, y).

R(y) :- {A1(x) or A2(x)}, Q(x, y).
Q(x, y) :- B(x, y), C(x, y).
 */

/*
fails to compile, because x cannot be bound in R
R(y) :- Q(x, y).
Q(x, y) :- demand(x), B(x, y), C(x, y).
to
R(y) :- demand(x), Q(x, y).
Q(x, y) :- inputGuard(x), B(x, y), C(x, y).


successfully compiles
R(y) :- Q(y, 0).
Q(x, y) :- demand(x), B(x, y), C(x, y).
to
R(y) :- demandHere(y), Q(y, 0).
Q(x, y) :- demandHere(x), B(x, y), C(x, y).

 */

trait Typechecker extends BaseIRTypechecker:
  override def checkParam(param: Param): Unit = param.ty match
    case TDemand(ty) =>
      registerVar(param.name, param, ty)
      bindVar(param.name)
    case _ => super.checkParam(param)

  private var ignoreDemand: Boolean = false

  def scopedIgnoreDemand[A](f: => A): A = {
    val before = ignoreDemand
    val t = f
    ignoreDemand = before
    t
  }

  override def checkAtom(atom: Atom, mode: Mode): Unit = scopedIgnoreDemand {
    ignoreDemand = ignoreDemand || atom.hasHint(Hints.IgnoreCallKey)
    super.checkAtom(atom, mode)
  }

  override def checkTerm(term: Term, expected: Type, mode: Mode): Mode = expected match
    case TDemand(ty) =>
      val newMode = if (ignoreDemand) mode else Mode.Bound
      checkTerm(term, ty, newMode)
    case _ => super.checkTerm(term, expected, mode)

  override def checkType(ty: Type): Unit = ty match
    case TDemand(tty) => checkType(tty)
    case _ => super.checkType(ty)