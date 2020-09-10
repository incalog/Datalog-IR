package inca.frontend.parser

import scala.collection.mutable
import scala.meta.{Case, Defn, Pat, Term, Tree}
import inca.frontend.core.Core.Name

object EvalHelper {

  /**
    * Computes the set of free(unbound) variables in this AST
    * @param term the AST
    * @return the set of free variables
    */
  def freeVars(term: Tree): scala.collection.immutable.Set[Name] = {
    loadVars(term, new Scope(mutable.Set(), mutable.Set())).free.toSet
  }

  /**
    * Loads the free variables of the whole AST and the variables bound at the level of the node
    * into the scope
    * @param term the AST
    * @param scope the scope of the AST node into which the variables are loaded
    * @return The modified scope.
    */
  private def loadVars(term: Tree, scope: Scope): Scope =
    term match {
      // definitions
      case Defn.Val(_, pats, _, rhs) =>
        val patVars = extractVars(pats, scope)
        // every pattern variable is a bound variable except for constants that might not be defined in the term
        // todo implement constants
        patVars.foreach(scope.newBound)
        loadVars(rhs, scope)
      case Defn.Var(_, pats, _, rhs) =>
        val patVars = extractVars(pats, scope)
        patVars.foreach(scope.newBound)
        if (rhs.isDefined) {
          loadVars(rhs.get, scope)
        } else {
          scope
        }

      // terms
      case Term.Name(name) =>
        if (scope.isBound(name) || scope.isFree(name)) {
          //we have already encountered this variable
          scope
        } else {
          // variable not already known => must be free
          scope.newFree(name)
        }

      case Term.ApplyType(fun, _)  => loadVars(fun, scope)
      case Term.Select(qual, _)    => loadVars(qual, scope)
      case Term.ApplyUnary(_, arg) => loadVars(arg, scope)
      case Term.Return(expr)       => loadVars(expr, scope)
      case Term.Annotate(expr, _)  => loadVars(expr, scope)
      case Term.Ascribe(expr, _)   => loadVars(expr, scope)
      case Term.Throw(expr)        => loadVars(expr, scope)
      case Term.Repeated(expr)     => loadVars(expr, scope)

      case Term.Interpolate(_, _, args) => foldFreeVars(args, scope)
      case Term.Tuple(args)             => foldFreeVars(args, scope)

      case Term.Block(stats) =>
        // a block defines a new scope nested in the current one
        foldFreeVars(stats, scope.nestedScope())
        scope

      case Term.ApplyInfix(lhs, _, _, args) => foldFreeVars(args, loadVars(lhs, scope))
      case Term.Apply(fun, args)            => foldFreeVars(args, loadVars(fun, scope))

      case Term.Function(params, body) =>
        params.foreach(p => scope.newBound(p.name.value))
        loadVars(body, scope)

      case Term.Assign(lhs, rhs) =>
        loadVars(rhs, loadVars(lhs, scope))

      case Term.Match(expr, cases) =>
        val scope1 = loadVars(expr, scope)
        cases.foreach(c => fromCase(c, scope1))
        scope1

      case Term.If(cond, thenp, elsep) =>
        loadVars(elsep, loadVars(thenp, loadVars(cond, scope)))

      case Term.New(init) =>
        init.argss.foreach(foldFreeVars(_, scope))
        scope

      case Term.PartialFunction(cases) =>
        cases.foreach(c => fromCase(c, scope))
        scope

      case Term.Try(expr, catchp, finallyp) =>
        loadVars(expr, scope)
        catchp.foreach(c => fromCase(c, scope))
        if (finallyp.isDefined) {
          loadVars(finallyp.get, scope)
        }
        scope

      case Term.TryWithHandler(expr, catchp, finallyp) =>
        loadVars(expr, scope)
        loadVars(catchp, scope)
        if (finallyp.isDefined) {
          loadVars(finallyp.get, scope)
        }
        scope

      case Term.NewAnonymous(templ) =>
        val nested = scope.nestedScope()
        foldFreeVars(templ.early, nested)
        foldFreeVars(templ.stats, nested)
        templ.inits.foreach(init => init.argss.foreach(foldFreeVars(_, nested)))
        scope

      case Term.Do(body, expr) =>
        loadVars(body, scope)
        loadVars(expr, scope)

      case Term.For(enums, body) =>
        val nested = scope.nestedScope()
        foldFreeVars(enums, nested)
        loadVars(body, nested)

      case Term.ForYield(enums, body) =>
        val nested = scope.nestedScope()
        foldFreeVars(enums, nested)
        loadVars(body, nested)

      case Term.While(expr, body) =>
        loadVars(expr, scope)
        loadVars(body, scope)

      case _ => scope
    }

  private def foldFreeVars(terms: List[Tree], scope: Scope): Scope = {
    terms.foreach(t => loadVars(t, scope))
    scope
  }

  private def extractVars(pats: List[Pat], scope: Scope): mutable.Set[Name] = {
    pats.flatMap(definedVars(_, scope)).to(mutable.Set)
  }

  /**
    * extracts the free variables defined in a pattern
    * @param pat the pattern
    * @return the set of free variables in the pattern
    */
  private def definedVars(pat: Pat, scope: Scope): mutable.Set[Name] =
    pat match {
      case Pat.Var(Term.Name(name)) => mutable.Set(name)
      case Pat.Bind(lhs, rhs)       => definedVars(lhs, scope) ++ definedVars(rhs, scope)
      case Pat.Tuple(args) =>
        args.foldLeft(mutable.Set[Name]()) {
          case (found, p) => found ++ definedVars(p, scope)
        }
      case Pat.Alternative(lhs, rhs) => definedVars(lhs, scope) ++ definedVars(rhs, scope)
      case Pat.Extract(fun, args) =>
        loadVars(fun, scope)
        extractVars(args, scope)
      case Pat.Typed(p, _) => definedVars(p, scope)
    }

  private def fromCase(cas: Case, scope: Scope): Unit = {
    val patVars = definedVars(cas.pat, scope)
    if (cas.cond.isDefined) {
      patVars.foreach(scope.newBound)
      loadVars(cas.cond.get, scope).free
    }
    loadVars(cas.body, scope.nestedScope())
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
