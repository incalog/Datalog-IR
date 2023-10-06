package inca.ir.extension.demand

import inca.ir.{Atom, Param, TNothing, Term, Type}
import inca.ir.extension.disjunction.Disjunction
import inca.ir.typing.{BaseIRTypechecker, Mode}

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
  override def typecheckParam(param: Param): Unit = param.ty match
    case TDemand(ty) =>
      registerVar(param.name, param, ty)
      bindVar(param.name)
    case _ => super.typecheckParam(param)

  override def checkTerm(term: Term, expected: Type, mode: Mode): Mode = expected match
    case TDemand(ty) => checkTerm(term, ty, Mode.Bound)
    case _ => super.checkTerm(term, expected, mode)