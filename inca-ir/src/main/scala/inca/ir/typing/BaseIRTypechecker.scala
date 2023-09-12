package inca.ir.typing

import inca.ir.util.SourceLocation
import inca.ir.*
import inca.ir.extension.data.DataDefinition
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.Seq

// We assume that every variable that is used is introduced beforehand (left-to-right)
trait BaseIRTypechecker extends BaseIRTypeContext:

  // Always process Relations last
  implicit def ordering[A <: ModuleEntry]: Ordering[A] = (x: A, y: A) => (x, y) match
    case (r1: Relation, r2: Relation) => 0
    case (_, r2: Relation) => -1
    case (r1: Relation, _) => 1
    case _ => 0

  def typecheck(program: Seq[Module]): Unit = scopedTypeContext {
    program.foreach(bindModule)
    program.foreach(typecheck)
  }

  def typecheck(module: Module): Unit = scopedTypeContext {
    // TODO: Bind EDB entries
    module.contents.sorted.foreach(bindModuleEntry)
    module.contents.sorted.foreach(typecheck)
    this.failOnError()
  }

  def typecheck(moduleEntry: ModuleEntry): Unit = moduleEntry match {
      case relation: Relation => scopedTypeContext { typecheck(relation) }
      case _ => throw IllegalArgumentException(s"Can not typecheck unknown entry: $moduleEntry")
  }

  def typecheck(relation: Relation): Unit = {
    // bind parameters
    relation.params.foreach { param =>
      registerVar(param.name, param, param.ty)
    }
    relation.bodies.foreach(b => scopedTypeContext {
      typecheck(b)
      relation.params.foreach { p =>
        if (!isBoundVar(p.name))
          error(s"Parameter $p is not positively bound in relation \"${relation.name}\", body \n$b")
      }
    })

  }

  def typecheck(body: Body): Unit =
    body.atoms.foreach(at => checkAtom(at, Mode.Closing))

  def assertComparable(ty: Type, outside: Type, t: SourceLocation): Unit = (ty, outside) match
    case (TAny, _) | (_, TAny) => // fine
    case (_, TNothing) => error(s"Expected type $outside, which cannot be inhabited by $t")
    case (TNothing, _) => error(s"Expected type $outside, but $t has type $ty")
    case _ =>
      if (ty == outside) {
        // fine
      } else {
        error(s"Expected type $outside, but $t has type $ty")
      }


  final def checkTerm(term: Term, expected: Type, mode: Mode): Unit =
    assignType(term) {
      checkTermExtend(term, expected, mode)
      expected
    }

  final def inferTerm(term: Term, mode: Mode): Type =
    assignType(term) {
      inferTermExtend(term, mode)
    }

  protected def checkTermExtend(term: Term, expected: Type, mode: Mode): Unit = term match
    case v@Var(name) => lookupVar(name) match
      case None if mode.requiresClosed =>
        error(s"Undefined variable $v at closed position", v)
        registerVar(name, v, TAny)
        bindVar(name)
      case Some(VarInfo(_, _, VarMode.Unbound)) if mode.requiresClosed =>
        error(s"Unbound variable $v not allowed here", v)
        bindVar(name)
      case None => // register and bind new variable
        registerVar(name, v, expected)
        bindVar(name)
      case Some(VarInfo(_, ty, _)) => // bind variable (if needed) and assure type compatibility
        bindVar(name)
        assertComparable(ty, expected, v)
    case Cast(t, ty) =>
      checkTerm(t, ty, mode)
      assertComparable(ty, expected, term)
    case _ => // fallback to infer + compatibility check
      val ty = inferTerm(term, mode)
      assertComparable(ty, expected, term)

  protected def inferTermExtend(term: Term, mode: Mode): Type = term match
    case v@Var(name) => lookupVar(name) match
      case None if mode.requiresClosed =>
        error(s"Undefined variable $v at closed position", v)
        registerVar(name, v, TAny)
        bindVar(name)
        TAny
      case Some(VarInfo(_, _, VarMode.Unbound)) if mode.requiresClosed =>
        error(s"Unbound variable $v not allowed here", v)
        bindVar(name)
        TAny
      case None => // register and bind new variable
        error(s"Cannot infer type of Undefined variable $v", v)
        registerVar(name, v, TAny)
        bindVar(name)
        TAny
      case Some(VarInfo(_, ty, _)) => // bind variable (if needed) and assure type compatibility
        bindVar(name)
        ty
    case Cast(t, ty) =>
      checkTerm(t, ty, mode)
      ty
    case _ => throw IllegalArgumentException(s"Can not typecheck unknown term: $term")

  def checkCall(name: Name, args: Seq[Term], atom: Atom, mode: Mode): Unit =
    lookupModuleEntry(name) match
      case Some(Relation(_, params, _)) =>
        if (args.size != params.size)
          error(s"Expected ${params.size} arguments but got: ${args.size}", atom)

        args.zip(params).foreach { case (t, Param(_, ty)) =>
          checkTerm(t, ty, mode)
        }
      case _ => error(s"Unknown relation: $name", atom)

  def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Call(name, args) => checkCall(name, args, atom, mode)
    case NegCall(name, args) => checkCall(name, args, atom, mode.inverted)
    case ExtensionalCall(name, args) => checkCall(name, args, atom, mode)
    case NegExtensionalCall(name, args) => checkCall(name, args, atom, mode.inverted)

    case Eq(lhs, rhs) =>
      val action = startContextTransaction()
      withErrors(inferTerm(lhs, Mode.Closed)) match
        case (lty, Nil) =>
          action.commit()
          checkTerm(rhs, lty, mode)
        case (_, lerrs) =>
          action.abort()
          withErrors(inferTerm(rhs, Mode.Closed)) match
            case (rty, Nil) => checkTerm(lhs, rty, mode)
            case (_, rerrs) =>
              error(s"Ill-typed equation, cannot infer closed type for either side", atom)
              lerrs.foreach(e => error(e.msg, e.sourceLocations:_*))
              rerrs.foreach(e => error(e.msg, e.sourceLocations:_*))

    case Neq(lhs, rhs) =>
      val action = startContextTransaction()
      withErrors(inferTerm(lhs, Mode.Closed)) match
        case (lty, Nil) =>
          action.commit()
          checkTerm(rhs, lty, mode.inverted)
        case (_, lerrs) =>
          action.abort()
          withErrors(inferTerm(rhs, Mode.Closed)) match
            case (rty, Nil) => checkTerm(lhs, rty, mode.inverted)
            case (_, rerrs) =>
              error(s"Ill-typed equation, cannot infer closed type for either side", atom)
              lerrs.foreach(e => error(e.msg, e.sourceLocations: _*))
              rerrs.foreach(e => error(e.msg, e.sourceLocations: _*))

    case _ =>
      throw IllegalStateException(s"Can not typecheck unknown atom: $atom")


  protected[ir] def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match {
    case (_, TAny) => true
    case (TNothing, _) => true
    case _ => ty1 == ty2
  }

  protected[ir] def join(ty1: Type, ty2: Type): Type =
    if (subtype(ty1, ty2))
      ty2
    else if (subtype(ty2, ty1))
      ty1
    else
      TAny

  protected[ir] def meet(ty1: Type, ty2: Type): Type = TAny

  protected[ir] def join(tys: Iterable[Type]): Type = tys.foldLeft[Type](TNothing)(join)

  protected[ir] def assertSubtype(ty1: Type, ty2: Type, location: SourceLocation*): Unit =
    if (!subtype(ty1, ty2))
      error(s"Expected $ty1 but got: $ty2", location: _*)

  protected[ir] def assignType(term: Typeable[Type] with SourceLocation)(computeType: => Type): Type =
    val inferred = computeType
    term.typed(inferred, force = true)
    inferred

