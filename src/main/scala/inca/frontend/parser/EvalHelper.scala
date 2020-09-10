package inca.frontend.parser

import scala.collection.mutable
import scala.meta.{Case, Defn, Enumerator, Lit, Pat, Term, Tree}
import inca.frontend.core.Core.Name

object EvalHelper {

  /**
   * Computes the set of free(unbound) variables in this AST
   * @param term the AST
   * @param initBound an optional initial set of bound variables
   * @return the set of free variables
   */
  def freeVars(term: Tree, initBound: Set[String] = Set()): scala.collection.immutable.Set[Name] = {
    val scope = new Scope(mutable.Set(), initBound.to(mutable.Set))
    loadVars(term, scope)
    scope.free.toSet
  }

  /**
   * Loads the free variables of the whole AST and the variables bound at the level of the node
   * into the scope
   * @param term the AST
   * @param scope the scope of the AST node into which the variables are loaded
   */
  private def loadVars(term: Tree, scope: Scope): Unit = term match {

    //literals don't contain variables
    case _: Lit =>

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
      if(rhs.isDefined) {
        loadVars(rhs.get, scope)
      }
    case _: Defn.Macro
         | _: Defn.Class
         | _: Defn.Object
         | _: Defn.Def
         | _: Defn.Trait
         | _: Defn.Type => throw new UnsupportedOperationException("only val and var are supported definitions")

    // terms
    case Term.Name(name) =>
      if(!scope.isBound(name) && !scope.isFree(name)) {
        // variable not already known => must be free
        scope.newFree(name)
      }

    case Term.ApplyType(fun, _) => loadVars(fun, scope)
    case Term.Select(qual, _) => loadVars(qual, scope)
    case Term.ApplyUnary(_, arg) => loadVars(arg, scope)
    case Term.Return(expr) => loadVars(expr, scope)
    case Term.Annotate(expr, annots) =>
      loadVars(expr, scope)
      annots.foreach(_.init.argss.foreach(loadAllVars(_, scope)))
    case Term.Ascribe(expr, _) => loadVars(expr, scope)
    case Term.Throw(expr) => loadVars(expr, scope)
    case Term.Repeated(expr) => loadVars(expr, scope)

    case Term.Interpolate(prefix, _, args) =>
      loadAllVars(args, scope)
      if(!scope.isBound(prefix.value)) {
        scope.newFree(prefix.value)
      }
    case Term.Tuple(args) => loadAllVars(args, scope)

    case Term.Block(stats) =>
      // a block defines a new scope nested in the current one
      loadAllVars(stats, scope.nestedScope())

    case Term.ApplyInfix(lhs, _, _, args) =>
      loadVars(lhs, scope)
      loadAllVars(args, scope)
    case Term.Apply(fun, args) =>
      loadVars(fun, scope)
      loadAllVars(args, scope)

    case Term.Function(params, body) =>
      val scope1 = scope.nestedScope()
      params.foreach(p => scope1.newBound(p.name.value))
      loadVars(body, scope1)

    case Term.Assign(lhs, rhs) =>
      loadVars(lhs, scope)
      loadVars(rhs, scope)

    case Term.Match(expr, cases) =>
      loadVars(expr, scope)
      cases.foreach(c => loadFromCase(c, scope))

    case Term.If(cond, thenp, elsep) =>
      loadVars(cond, scope)
      loadVars(thenp, scope)
      loadVars(elsep, scope)

    case Term.New(init) =>
      init.argss.foreach(loadAllVars(_, scope))

    case Term.PartialFunction(cases) =>
      cases.foreach(c => loadFromCase(c, scope))

    case Term.Try(expr, catchp, finallyp) =>
      loadVars(expr, scope)
      catchp.foreach(c => loadFromCase(c, scope))
      if(finallyp.isDefined) {
        loadVars(finallyp.get, scope)
      }

    case Term.TryWithHandler(expr, catchp, finallyp) =>
      loadVars(expr, scope)
      loadVars(catchp, scope)
      if(finallyp.isDefined){
        loadVars(finallyp.get, scope)
      }

    case Term.NewAnonymous(_) => throw new UnsupportedOperationException("NewAnonymous currently not supported")

    case Term.Do(body, expr) =>
      loadVars(body, scope)
      loadVars(expr, scope)

    case Term.For(enums, body) =>
      val nested = scope.nestedScope()
      enums.foreach(loadFromEnumerator(_, nested))
      loadVars(body, nested)

    case Term.ForYield(enums, body) =>
      val nested = scope.nestedScope()
      enums.foreach(loadFromEnumerator(_, nested))
      loadVars(body, nested)

    case Term.While(expr, body) =>
      loadVars(expr, scope)
      loadVars(body, scope)

    case Term.Xml(_, args) => loadAllVars(args, scope)

    case Term.Super(_, _)
         | Term.This(_) =>

    case ex => throw new UnsupportedOperationException(s"not yet implemented: $ex")
  }

  private def loadAllVars(terms: List[Tree], scope: Scope): Unit = {
    terms.foreach(t => loadVars(t, scope))
  }

  private def extractVars(pats: List[Pat], scope: Scope): mutable.Set[Name] = {
    pats.flatMap(definedVars(_, scope)).to(mutable.Set)
  }

  private def loadFromEnumerator(enum: Enumerator, scope: Scope): Unit = {
    enum match {
      case Enumerator.Generator(pat, rhs) =>
        val patVars = definedVars(pat, scope)
        loadPatVars(patVars, scope)
        loadVars(rhs, scope)
      case Enumerator.Val(pat, rhs) =>
        val patVars = definedVars(pat, scope)
        patVars.foreach(scope.newBound)
        loadVars(rhs, scope)
      case Enumerator.Guard(cond) =>
        loadVars(cond, scope)
      case Enumerator.Quasi(_, _) => throw new UnsupportedOperationException("Enumerator.Quasi not currently supported")
    }
  }

  /**
   * extracts the free variables defined in a pattern
   * @param pat the pattern
   * @return the set of free variables in the pattern
   */
  private def definedVars(pat: Pat, scope: Scope): mutable.Set[Name] = pat match {
    case _: Lit => mutable.Set()
    case Pat.Var(Term.Name(name)) => mutable.Set(name)
    case Pat.Bind(lhs, rhs) => definedVars(lhs, scope) ++ definedVars(rhs, scope)
    case Pat.Tuple(args) => args.foldLeft(mutable.Set[Name]()) {
      case (found, p) => found ++ definedVars(p, scope)
    }
    case Pat.Alternative(lhs, rhs) => definedVars(lhs, scope) ++ definedVars(rhs, scope)
    case Pat.Extract(fun, args) =>
      loadVars(fun, scope)
      extractVars(args, scope)
    case Pat.Typed(p, _) => definedVars(p, scope)
    case Pat.Wildcard() => mutable.Set()
    case Pat.SeqWildcard() => mutable.Set()
    case Pat.Quasi(_, _) => throw new UnsupportedOperationException("Pat.Quasi is currently not supported")
    case Pat.Xml(_, _) => throw new UnsupportedOperationException("Pat.XML is not supported")
    case Term.Select(Term.Name(name), _) =>
      mutable.Set(name)
    case _ => throw new UnsupportedOperationException(s"not yet implemented: $pat")
  }

  private def loadPatVars(vars: mutable.Set[String], scope: Scope): Unit = {
    vars.foreach(
      v =>
        if(v.charAt(0).isUpper && !scope.isBound(v)) {
          // pattern variables with the first char in upper case are assumed to be constants defined somewhere else
          scope.newFree(v)
        }
        else {
          scope.newBound(v)
        }
    )
  }

  private def loadFromCase(cas: Case, scope: Scope): Unit = {
    val patVars = definedVars(cas.pat, scope)
    patVars.foreach(scope.newBound)
    if(cas.cond.isDefined) {
      loadVars(cas.cond.get, scope)
    }
    loadVars(cas.body, scope.nestedScope())
  }

  /**
   * A scope represents the free and bound variables in a specific code block.
   * Blocks for example create a new scope nested in the current one
   * so variables newly bound in this nested scope are only bound there and not in it's parent scope.
   * For the purpose of finding all free variables in an given AST this implementation uses only one set of free vars.
   * This means once an initial scope for the root of the tree is created, it and all it's nested scopes share this set
   * while the set of bound variables is cloned each time a new nested scope is created
   * @param free the set of free variables known in this scope
   * @param bound the set of bound variables known in this scope
   */
  private class Scope(val free: mutable.Set[Name], val bound: mutable.Set[Name]) {

    def isFree(v: Name): Boolean = free(v)

    def isBound(v: Name): Boolean = bound(v)

    /**
     * Adds the name to the set of known free variables.
     * Does nothing if the name is already referencing a known free variable
     * @param name the name of the new free variable
     */
    def newFree(name: Name): Unit = {
      free += name
    }

    /**
     * Adds the name to the set of known bound variables.
     * Does nothing if the name is already referencing a known bound variable
     * @param name the name of the new bound variable
     */
    def newBound(name: Name): Unit = {
      bound += name
    }

    def nestedScope(): Scope = {
      new Scope(free, bound.clone())
    }
  }
}
