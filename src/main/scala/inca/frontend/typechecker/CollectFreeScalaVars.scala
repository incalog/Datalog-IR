package inca.frontend.typechecker

import inca.frontend.core.Name

import scala.collection.mutable
import scala.meta.{Case, Decl, Defn, Enumerator, Import, Importee, Lit, Pat, Stat, Template, Term}

/**
 * The CollectFreeScalaVars contains methods to analyze the Scala code in Eval constructs
 *
 * @author Ronja Schnur (rschnur@students.uni-mainz.de)
 *         Julian Cichorius (jcichori@students.uni-mainz.de)
 * @version 0.0.1
 * @todo unfinished
 */
object CollectFreeScalaVars {

  val javaLang = Seq(
    "Boolean", "Character", "ClassLoader", "Double", "Enum", "Float", "Integer",
    "Long", "Number", "Math", "Object", "Package", "Process", "ProcessBuilder",
    "Runtime", "RuntimePremission", "SecurityManager", "Short",
    "StackTraceElement", "StrictMath", "String", "StringBuffer", "StringBuilder",
    "System", "Thread", "ThreadGroup", "ThreadLocal", "Throwable", "Void")

  val scalaPredef = Seq(
    "classOf", "valueOf", "String", "Class", "Function", "Map", "Set", "Seq",
    "List", "Nil", "::", "Manifest", "NoManifest", "manifest", "optManifest",
    "Option", "Some", "None",
    "identity", "implicitly", "locally", "assert", "assume", "require",
    "ArrowAssoc", "Ensuring", "StringFormat", "any2stringadd", "SeqCharSequence",
    "ArrayCharSequence", "augmentString", "print", "println", "printf", "mutable")

  val predefinedNames = javaLang ++ scalaPredef

  /**
   * Computes the set of free(unbound) variables in this AST.
   * Important note:
   * using variables with backtickets in patterns does not work properly with the computation of free vars.
   * The variable will be counted as bound even if it's meant to reference an undefined constant
   *
   * @param term      the AST
   * @param bound an optional initial set of bound variables
   * @return the set of free variables
   */
  def freeVars(term: Term, bound: Set[Name] = Set()): Set[Name] = {
    val extendedBound = mutable.Set[Name]() ++ bound
//    extendedBound ++= (bound ++ predefinedNames.map(Name))
    val scope = new Scope(mutable.Set(), extendedBound)
    freeVars(term, scope)
    scope.free.toSet
  }

  /**
   * Loads the free variables of the whole AST and the variables bound at the level of the node
   * into the scope
   *
   * @param term  the AST
   * @param scope the scope of the AST node into which the variables are loaded
   */

  private def freeVars(term: Term, scope: Scope): Unit = term match {
    case _: Lit => // nothing
    case tn: Term.Name =>
      val name = makeName(tn)
      if (!scope.isBound(name))
        scope.newFree(name)
    case Term.Match(matchee, cases) =>
      freeVars(matchee, scope)
      cases.foreach(freeVars(_, scope))
    case Term.Apply(fun, args) =>
      freeVars(fun, scope)
      args.foreach(freeVars(_, scope))
    case Term.ApplyInfix(lhs, op, _, args) =>
      freeVars(lhs, scope)
      args.foreach(freeVars(_, scope))
    case Term.Tuple(terms) => terms.foreach(freeVars(_, scope))
    case Term.Block(stats) =>
      val newScope = scope.nestedScope()
      stats.foreach(freeVars(_, newScope))
    case Term.For(enumerator, body) =>
      enumerator.foreach(freeVars(_, scope))
      val newScope = scope.nestedScope()
      freeVars(body, newScope)
    case Term.While(cond, body) =>
      freeVars(cond, scope)
      freeVars(body, scope.nestedScope())
    case Term.Do(body, cond) =>
      freeVars(cond, scope)
      freeVars(body, scope.nestedScope())
    case Term.If(cond, thn, els) =>
      freeVars(cond, scope)
      freeVars(thn, scope.nestedScope())
      freeVars(els, scope.nestedScope())
    case Term.ApplyType(fun, _) => freeVars(fun, scope)
    case Term.Select(qual, name) =>
      freeVars(qual, scope)
    // TODO implement
    case Term.ApplyUnary(_, arg) => freeVars(arg, scope)
    case Term.Return(expr) => freeVars(expr, scope)
    case Term.Annotate(expr, anno) =>
      anno.foreach(a => a.init.argss.flatten.foreach(freeVars(_, scope)))
      freeVars(expr, scope)
    case Term.Ascribe(expr, _) => freeVars(expr, scope)
    case Term.Throw(expr) => freeVars(expr, scope)
    case Term.Repeated(expr) => freeVars(expr, scope)
    case Term.Interpolate(prefix, _, args) =>
      prefix.value match {
        case "s" => // nothing
        case "f" => // nothing
        case "raw" => // nothing
        case _ => throw new IllegalArgumentException("Cannot support interpolation")
      }
      args.foreach(freeVars(_, scope))
    case Term.Function(params, body) =>
      val newScope = scope.nestedScope()
      params.foreach(p => newScope.newBound(makeName(p.name)))
      freeVars(body, newScope)
    case Term.Assign(lhs, rhs) =>
      freeVars(lhs, scope)
      freeVars(rhs, scope)
    case Term.ForYield(enums, body) =>
      val enumScope = scope.nestedScope()
      enums.foreach(freeVars(_, enumScope))
      val bodyScope = enumScope.nestedScope()
      freeVars(body, bodyScope)
    case Term.Super(_, _) | Term.This(_) | Term.Placeholder() =>
    case Term.Eta(expr) => freeVars(expr, scope)
    case Term.New(init) => init.argss.flatten.foreach(freeVars(_, scope))
    case Term.NewAnonymous(tmpl) =>
      freeVars(tmpl, scope)
    case Term.PartialFunction(cases) => cases.foreach(freeVars(_, scope))
    case Term.Try(expr, catchp, finallyp) =>
      freeVars(expr, scope)
      catchp.foreach(freeVars(_, scope))
      finallyp.foreach(freeVars(_, scope))
    case Term.TryWithHandler(expr, catchp, finallyp) =>
      freeVars(expr, scope)
      freeVars(catchp, scope)
      finallyp.foreach(freeVars(_, scope))
  }

  private def freeVars(defn: Defn, scope: Scope): Unit = defn match {
    case Defn.Val(_, pats, _, rhs) =>
      val bounded = pats.flatMap(freeVars(_, scope))
      bounded.foreach(scope.newBound)
      freeVars(rhs, scope)
    case Defn.Var(_, pats, _, rhs) =>
      val bounded = pats.flatMap(freeVars(_, scope))
      bounded.foreach(scope.newBound)
      rhs.foreach(freeVars(_, scope))
    case Defn.Def(_, name, _, paramss, _, body) =>
      scope.newBound(makeName(name))
      val newScope = scope.nestedScope()
      paramss.foreach { params =>
        params.foreach { p => newScope.newBound(makeName(p.name)) }
      }
      freeVars(body, newScope)
    case Defn.Type(_, lhs, _, rhs) => // nothing
    case Defn.Trait(_, name, _, ctor, tmpl) =>
      scope.newBound(makeName(name))
      // TODO prorcess ctor
      freeVars(tmpl, scope)
    case Defn.Class(_, name, _, ctor, tmpl) =>
      scope.newBound(makeName(name))
      // TODO prorcess ctor
      freeVars(tmpl, scope)
    case Defn.Object(_, name, tmpl) =>
      scope.newBound(makeName(name))
      freeVars(tmpl, scope)
  }

  private def freeVars(tmpl: Template, scope: Scope): Unit = {
    tmpl.inits.foreach { init =>
      init.argss.flatten.foreach(freeVars(_, scope))
    }
    val newScope = scope.nestedScope()
    // TODO self and early
    tmpl.stats.foreach(freeVars(_, newScope))
  }

  private def freeVars(stat: Stat, scope: Scope): Unit = stat match {
    case decl: Decl => freeVars(decl, scope)
    case defn: Defn => freeVars(defn, scope)
    case term: Term => freeVars(term, scope)
    case Import(importers) =>
      importers.foreach { imp =>
        imp.importees.foreach {
          case Importee.Name(n) =>
            scope.newBound(makeName(n))
          case Importee.Rename(_, n) =>
            scope.newBound(makeName(n))
          case _: Importee.Unimport => // nothing
          case _: Importee.Wildcard =>
            // wildcards are not allowed
            throw new IllegalArgumentException("Wildcard Scala imports are now allowed")
          case _ =>
            throw new IllegalArgumentException("Unsupported import in eval")
        }
      }
  }

  private def freeVars(decl: Decl, scope: Scope): Unit = throw new IllegalArgumentException("Decls are not supported")

  private def freeVars(cas: Case, scope: Scope): Unit = {
    val bound = freeVars(cas.pat, scope)
    val newScope = scope.nestedScope()
    bound.foreach(newScope.newBound)
    cas.cond.foreach(freeVars(_, newScope))
    freeVars(cas.body, newScope)
  }

  private def freeVars(enum: Enumerator, scope: Scope): Unit = enum match {
    case Enumerator.Generator(pat, rhs) =>
      freeVars(rhs, scope)
      val bound = freeVars(pat, scope)
      bound.foreach(scope.newBound)
    case Enumerator.Guard(cond) =>
      freeVars(cond, scope)
    case Enumerator.Val(pat, rhs) =>
      freeVars(rhs, scope)
      val bound = freeVars(pat, scope)
      bound.foreach(scope.newBound)
  }

  def makeName(tn: meta.Name): Name = {
    val n = Name(tn.value)
    n.startIndex = tn.pos.start
    n.endIndex = tn.pos.end
    n
  }

  private def freeVars(pat: Pat, scope: Scope): Set[Name] = pat match {
    case Pat.Wildcard() | Pat.SeqWildcard() | _: Lit => Set()
    case Pat.Var(tn) => Set(makeName(tn))
    case Term.Select(qual, _) => qual match {
        case tn: Term.Name => Set(makeName(tn))
        case inner: Term.Select => freeVars(inner, scope)
      }
    case tn: Term.Name => Set(makeName(tn))
    case Pat.Bind(lhs, rhs) => freeVars(lhs, scope) ++ freeVars(rhs, scope)
    case Pat.Tuple(args) => args.flatMap(freeVars(_, scope)).toSet
    case Pat.Alternative(lhs, rhs) => freeVars(lhs, scope) ++ freeVars(rhs, scope)
    case Pat.Extract(fun, args) =>
      freeVars(fun, scope)
      args.flatMap(freeVars(_, scope)).toSet
    case Pat.ExtractInfix(lhs, op, rhs) =>
      val bound = freeVars(lhs, scope)
//      freeVars(op, scope)
      bound ++ rhs.flatMap(freeVars(_, scope))
    case Pat.Typed(p, _) => freeVars(p, scope)
    case Pat.Quasi(_, _) => throw new UnsupportedOperationException("Pat.Quasi is currently not supported")
    case Pat.Xml(_, _) => throw new UnsupportedOperationException("Pat.XML is not supported")
    case _ => throw new UnsupportedOperationException(s"not yet implemented: ${pat.productPrefix}")
  }


  private def addPatVar(v: Name, scope: Scope): Unit =
    if (v.name.charAt(0).isUpper && !scope.isBound(v)) {
      // pattern variables with the first char in upper case are assumed to be constants defined somewhere else
      scope.newFree(v)
    } else {
      scope.newBound(v)
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

    def isFree(v: Name): Boolean = free.exists(_.name == v.name)

    def isBound(v: Name): Boolean = bound.exists(_.name == v.name)

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
}
