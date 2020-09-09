package inca.frontend.parser

import scala.collection.mutable
import scala.meta.{Case, Defn, Pat, Term, Tree}
import inca.frontend.core.Core.Name

object EvalHelper {

  def freeVars(term: Tree): scala.collection.immutable.Set[Name] = {
    _freeVars(term, new Scope(mutable.Set(), mutable.Set())).free.toSet
  }

  private def _freeVars(term: Tree, scope: Scope): Scope = term match {
    case Defn.Val(_, pats, _, rhs) =>
      val patVars = extractVars(pats)
      patVars.foreach(scope.newBound)
      _freeVars(rhs, scope)
    case Defn.Var(_, pats, _, rhs) =>
      val patVars = extractVars(pats)
      patVars.foreach(scope.newBound)
      if(rhs.isDefined) {
        _freeVars(rhs.get, scope)
      } else {
        scope
      }
    case Term.ApplyType(fun, _) => _freeVars(fun, scope)
    case Term.Name(name) => if(scope.isBound(name)) scope else scope.newFree(name)
    case Term.Select(qual, _) => _freeVars(qual, scope)
    case Term.ApplyUnary(_, arg) => _freeVars(arg, scope)
    case Term.Block(stats) =>
      foldFreeVars(stats, scope.nestedScope())
      scope
    case Term.ApplyInfix(lhs, _, _, args) =>
      foldFreeVars(args, _freeVars(lhs, scope))
    case Term.Apply(fun, args) => foldFreeVars(args, _freeVars(fun, scope))
    case Term.Function(params, body) =>
      params.foreach(p => scope.newBound(p.name.value))
      _freeVars(body, scope)
    case Term.Assign(lhs, rhs) =>
      lhs match {
        case Term.Name(name) =>
          if(scope.isBound(name) || scope.isFree(name))
            _freeVars(rhs, scope)
          else
            _freeVars(rhs, scope.newFree(name))
        case _ => _freeVars(rhs, _freeVars(lhs, scope))
      }
    case Term.Match(expr, cases) =>
      val scope1 = _freeVars(expr, scope)
      cases.foreach(c => fromCase(c, scope1))
      scope1
    case Term.If(cond, thenp, elsep) =>
      _freeVars(elsep, _freeVars(thenp, _freeVars(cond, scope)))
    case Term.New(init) =>
      init.argss.foreach(foldFreeVars(_, scope))
      scope
    case Term.Return(expr) => _freeVars(expr, scope)
    case _ => scope
  }

  private def foldFreeVars(terms: List[Tree], scope: Scope): Scope = terms match {
    case Nil => scope
    case t :: ts => ts.foldLeft(_freeVars(t, scope)) {
      case (scope1, t) => _freeVars(t, scope1)
    }
  }

  private def extractVars(pats: List[Pat]): mutable.Set[Name] = {
    pats.flatMap(definedVars).to(mutable.Set)
  }

  private def definedVars(pat: Pat): mutable.Set[Name] = pat match {
    case Pat.Var(Term.Name(name)) => mutable.Set(name)
    case Pat.Bind(lhs, rhs) => definedVars(lhs) ++ definedVars(rhs)
    case Pat.Tuple(args) => args.foldLeft(mutable.Set[Name]()) {
      case (found, p) => found ++ definedVars(p)
    }
  }

  private def fromCase(cas: Case, scope: Scope): Unit = {
    val patVars = definedVars(cas.pat)
    if(cas.cond.isDefined) {
      patVars.foreach(scope.newBound)
      _freeVars(cas.cond.get, scope).free
    }
    _freeVars(cas.body, scope.nestedScope())
  }

  private class Scope(val free: mutable.Set[Name], val bound: mutable.Set[Name]) {

    def isFree(v: Name): Boolean = free(v)

    def isBound(v: Name): Boolean = bound(v)

    def newFree(f: Name): Scope = {
      free += f
      this
    }

    def newBound(b: Name): Scope = {
      bound += b
      this
    }

    def nestedScope(): Scope = {
      new Scope(free, bound.clone())
    }
  }
}
