package inca.frontend.util

import inca.frontend.core.Core._
import inca.frontend.typechecker.TypeContext

import scala.collection.mutable
import scala.meta.Type.Param
import scala.meta.{Case, Defn, Enumerator, Lit, Pat, Term, Tree, Type}
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.{ToolBox, ToolBoxError}

/**
 * The EvalHelper contains methods to analyze the Scala code in Eval constructs
 *
 * @author Ronja Schnur (rschnur@students.uni-mainz.de)
 *         Julian Cichorius (jcichori@students.uni-mainz.de)
 * @version 0.0.1
 * @todo unfinished
 */
object EvalHelper {

  /**
   * Computes the set of free(unbound) variables in this AST.
   * Important note:
   * using variables with backtickets in patterns does not work properly with the computation of free vars.
   * The variable will be counted as bound even if it's meant to reference an undefined constant
   *
   * @param term      the AST
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
   *
   * @param term  the AST
   * @param scope the scope of the AST node into which the variables are loaded
   */
  private def loadVars(term: Tree, scope: Scope): Unit = term match {

    //literals don't contain variables
    case _: Lit =>

    // definitions
    case Defn.Val(_, pats, _, rhs) =>
      val patVars = extractVars(pats, scope)
      loadPatVars(patVars, scope)
      loadVars(rhs, scope)

    case Defn.Var(_, pats, _, rhs) =>
      val patVars = extractVars(pats, scope)
      loadPatVars(patVars, scope)
      rhs.foreach(loadVars(_, scope))

    case _: Defn.Type =>

    case _: Defn =>
      throw new UnsupportedOperationException("only val, var anf type definitions are supported definitions")

    // terms
    case Term.Name(name) =>
      if (!scope.isBound(name) && !scope.isFree(name)) {
        // variable not already known => must be free
        scope.newFree(name)
      }

    case Term.ApplyType(fun, _) =>
      loadVars(fun, scope)

    case Term.Select(qual, _) =>
      loadVars(qual, scope)

    case Term.ApplyUnary(_, arg) =>
      loadVars(arg, scope)

    case Term.Return(expr) =>
      loadVars(expr, scope)

    case Term.Annotate(expr, anno) =>
      anno.foreach(a => a.init.argss.foreach(loadAllVars(_, scope)))
      loadVars(expr, scope)

    case Term.Ascribe(expr, _) =>
      loadVars(expr, scope)

    case Term.Throw(expr) =>
      loadVars(expr, scope)

    case Term.Repeated(expr) =>
      loadVars(expr, scope)

    case Term.Interpolate(prefix, _, args) =>
      loadAllVars(args, scope)
      scope.newFreeIfUnbound(prefix.value)

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
      finallyp.foreach(loadVars(_, scope))

    case Term.TryWithHandler(expr, catchp, finallyp) =>
      loadVars(expr, scope)
      loadVars(catchp, scope)
      finallyp.foreach(loadVars(_, scope))

    case Term.NewAnonymous(_) =>
      throw new UnsupportedOperationException("NewAnonymous currently not supported")

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

    case Term.Xml(_, args) =>
      loadAllVars(args, scope)

    case Term.Super(_, _)
         | Term.This(_)
         | Term.Placeholder() =>

    case Term.Eta(expr) =>
      loadVars(expr, scope)

    case tree =>
      throw new UnsupportedOperationException(s"not yet implemented: ${tree.productPrefix}")
  }

  private def loadAllVars(terms: List[Tree], scope: Scope): Unit = {
    terms.foreach(t => loadVars(t, scope))
  }

  private def extractVars(pats: List[Pat], scope: Scope): mutable.Set[Name] = {
    val vars = mutable.Set[Name]()
    loadFromPatterns(pats, scope, vars)
    vars
  }

  private def loadFromPatterns(pats: List[Pat], scope: Scope, found: mutable.Set[Name]): Unit = {
    pats.foreach(loadDefinedVars(_, scope, found))
  }

  private def loadFromEnumerator(enum: Enumerator, scope: Scope): Unit = enum match {

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

    case Enumerator.Quasi(_, _) =>
      throw new UnsupportedOperationException("Enumerator.Quasi not currently supported")
  }

  /**
   * extracts the free variables defined in a pattern
   *
   * @param pat   the pattern
   * @param scope the scope the pattern is defined in
   * @return the set of free variables in the pattern
   */
  private def definedVars(pat: Pat, scope: Scope): mutable.Set[Name] = {
    val vars = mutable.Set[Name]()
    loadDefinedVars(pat, scope, vars)
    vars
  }

  private def loadDefinedVars(pat: Pat, scope: Scope, found: mutable.Set[Name]): Unit = pat match {

    case Pat.Wildcard()
         | Pat.SeqWildcard()
         | _: Lit =>

    case Pat.Var(Term.Name(name)) =>
      found += name

    case Term.Select(qual, _) =>
      qual match {
        case Term.Name(name) =>
          found += name
        case inner: Term.Select =>
          loadDefinedVars(inner, scope, found)
      }

    case Term.Name(name) =>
      found += name

    case Pat.Bind(lhs, rhs) =>
      loadDefinedVars(lhs, scope, found)
      loadDefinedVars(rhs, scope, found)

    case Pat.Tuple(args) =>
      args.foreach(loadDefinedVars(_, scope, found))

    case Pat.Alternative(lhs, rhs) =>
      loadDefinedVars(lhs, scope, found)
      loadDefinedVars(rhs, scope, found)

    case Pat.Extract(fun, args) =>
      loadVars(fun, scope)
      loadFromPatterns(args, scope, found)

    case Pat.ExtractInfix(lhs, op, rhs) =>
      loadDefinedVars(lhs, scope, found)
      loadVars(op, scope)
      rhs.foreach(loadDefinedVars(_, scope, found))

    case Pat.Typed(p, _) =>
      loadDefinedVars(p, scope, found)

    case Pat.Quasi(_, _) =>
      throw new UnsupportedOperationException("Pat.Quasi is currently not supported")

    case Pat.Xml(_, _) =>
      throw new UnsupportedOperationException("Pat.XML is not supported")

    case _ =>
      throw new UnsupportedOperationException(s"not yet implemented: ${pat.productPrefix}")
  }

  private def loadPatVars(vars: mutable.Set[String], scope: Scope): Unit = {
    vars.foreach(
      v =>
        if (v.charAt(0).isUpper && !scope.isBound(v)) {
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
    loadPatVars(patVars, scope)
    cas.cond.foreach(loadVars(_, scope))
    loadVars(cas.body, scope.nestedScope())
  }

  /**
   * A scope represents the free and bound variables in a specific code block.
   * Blocks for example create a new scope nested in the current one
   * so variables newly bound in this nested scope are only bound there and not in it's parent scope.
   * For the purpose of finding all free variables in an given AST this implementation uses only one set of free vars.
   * This means once an initial scope for the root of the tree is created, it and all it's nested scopes share this set
   * while the set of bound variables is cloned each time a new nested scope is created
   *
   * @param free  the set of free variables known in this scope
   * @param bound the set of bound variables known in this scope
   */
  private class Scope(val free: mutable.Set[Name], val bound: mutable.Set[Name]) {

    def isFree(v: Name): Boolean = free(v)

    def isBound(v: Name): Boolean = bound(v)

    /**
     * Adds the name to the set of known free variables.
     * Does nothing if the name is already referencing a known free variable
     *
     * @param name the name of the new free variable
     */
    def newFree(name: Name): Unit = {
      free += name
    }

    /**
     * Adds the name to the set of known bound variables.
     * Does nothing if the name is already referencing a known bound variable
     *
     * @param name the name of the new bound variable
     */
    def newBound(name: Name): Unit = {
      bound += name
    }

    def nestedScope(): Scope = {
      new Scope(free, bound.clone())
    }

    def newFreeIfUnbound(name: Name): Unit = {
      if (!isBound(name)) {
        newFree(name)
      }
    }
  }

  /**
   * Computes the result type of an Eval expression and validates the contained Scala code for type correctness
   * However it does not set the type of the eval expression. This is up to the caller
   * @param eval the eval expression
   * @param ctx the type context of the type check run
   * @return the type annotation of the result type
   */
  def typecheck(eval: Eval)(implicit ctx: TypeContext): TypeAnno = {
    val params = eval.params
    // if we didn't find the free variable it might be a package so we ignore it and let the compiler figure it out
    val env = params.filter(ctx.tenv.contains).map(p => p -> ctx.tenv(p))
    // here we use a little hack. We create one big block that defines all the params with their type
    // but because they need to be initialized as well we simply throw an exception everytime
    // because throw is an expression that results in the bottom type Nothing so the typechecker is happy
    val paramString = env.map {
      case (name, typ) => s"val $name : ${typ.prettyprint} = throw new Exception()"
    }.mkString("; ")
    val codeSource = s"{$paramString; ${eval.code.syntax}}"
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(codeSource)
    try {
      val typechecked = toolbox.typecheck(tree)
      val typ = typechecked.tpe.dealias
      TypeHelper.decode(typ.toString)
    } catch {
      // throw a different exception to hide impl details
      case ToolBoxError(msg, _) => throw ScalaTypeError(msg)
    }
  }



}

case class ScalaTypeError(msg: String) extends Exception(msg)
