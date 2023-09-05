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

  def typecheck(body: Body): Unit = body.atoms.foreach(typecheck)

  private def typecheckCall(name: Name, args: Seq[Term], atom: Atom, positive: Boolean): Unit = {
    lookupModuleEntry(name) match
      case Some(Relation(_, params, _)) =>
        if (args.size != params.size)
          error(s"Expected ${params.size} arguments but got: ${args.size}", atom)

        val paramTys = params.map(_.ty)
        // Assign a type to a variable in case it was previously unbound
        args.zip(paramTys).foreach {
          case (arg@Var(name), paramTy) if arg.typ.isEmpty => assignType(arg)(paramTy)
          case (arg, paramTy) => // Nothing
        }
        // typecheck all args and thereby bind any missing variables
        val bound = Bound(positive)
        val argTys = args.map(typecheck(_, bound))
        args.zip(argTys).zip(paramTys).foreach { case ((a, aTy), pTy) =>
          assertSubtype(pTy, aTy, a, atom)
        }

      case _ => error(s"Unknown relation: $name", atom)
  }

  private def typecheckExtensionalCall(name: Name, args: Seq[Term], atom: Atom, positive: Boolean): Unit = {
    args.foreach {
      case v@Var(name) if lookupVar(name).isEmpty =>
        warn(s"Unregistered variable $name in extensional call, inferring TAny", atom)
        TAny
      case v => // Nothing
    }
    val bound = Bound(positive)
    args.foreach(typecheck(_, bound))
  }

  def typecheck(atom: Atom): Unit = atom match {
    case Call(name, args) =>
      typecheckCall(name, args, atom, true)
    case NegCall(name, args) =>
      typecheckCall(name, args, atom, false)
    case ExtensionalCall(name, args) =>
      typecheckExtensionalCall(name, args, atom, true)
    case NegExtensionalCall(name, args) =>
      typecheckExtensionalCall(name, args, atom, false)

    case Eq(lhs, rhs) =>
      val lhsIsFree = isFreeVar(lhs)
      val rhsIsFree = isFreeVar(rhs)

      if (lhsIsFree && rhsIsFree) {
        error(s"Neither $lhs nor $rhs is positively bound, comparison not possible", atom)
      } else if (lhsIsFree) {
        val rhsTy = typecheck(rhs, Bound.Assert)
        // assign the expected type for unbound vars
        assignType(lhs)(rhsTy)
        typecheck(lhs, Bound.Assign)
      } else if (rhsIsFree) {
        val lhsTy = typecheck(lhs, Bound.Assert)
        // assign the expected type for unbound vars
        assignType(rhs)(lhsTy)
        typecheck(rhs, Bound.Assign)
      } else {
        val lhsTy = typecheck(lhs, Bound.Assert)
        val rhsTy = typecheck(rhs, Bound.Assert)
        if (!subtype(lhsTy, rhsTy) && !subtype(rhsTy, lhsTy))
          warn("Comparing unrelated types will always fail!", atom)
      }

    case Neq(lhs, rhs) =>
      val lhsTy = typecheck(lhs, Bound.Assert)
      val rhsTy = typecheck(rhs, Bound.Assert)
      assertSubtype(lhsTy, rhsTy, atom)

    case _ =>
      throw IllegalStateException(s"Can not typecheck unknown atom: $atom")
  }

  enum Bound:
    case Assert
    case Assign

  object Bound:
    def apply(positive: Boolean): Bound = if (positive) Bound.Assign else Bound.Assert

  final def typecheck(term: Term, bound: Bound): Type = assignType(term) {
    term match
      case v@Var(name) => bound match
        case Bound.Assert => lookupVar(name) match
          case None | Some(VarInfo(_, _, false)) =>
            error(s"Expected positively bound variable, but $name is unbound", term)
            TAny
          case Some(info) => info.ty
        case Bound.Assign => lookupVar(name) match
          case Some(VarInfo(_, ty, positive)) =>
            if (!positive)
              bindVar(name)
            ty
          case None => term.typ match
            case None =>
              error(s"Cannot infer type of variable $name", term)
              TAny
            case Some(ty) =>
              registerVar(name, v, ty)
              bindVar(name)
              ty
      case _ => typecheckInternal(term, term.typ, bound)
  }

  protected[ir] def typecheckInternal(term: Term, inferred: Option[Type], bound: Bound): Type = term match {
    case v@Var(name) =>
      (lookupVar(name), inferred) match {
        case (Some(info), _) => // lookup was a bound variable or a param
          info.ty
        case (None, Some(ty)) => // inferred a type for an unbound variable
          registerVar(name, v, ty)
          ty
        case (None, None) => // unbound variable, but we could not infer a type
          error(s"Unregistered variable $name", term)
          TAny
      }
    case _ => throw IllegalArgumentException(s"Can not typecheck unknown term: $term")
  }

  protected[ir] def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match {
    case (_, TAny) => true
    case (TNothing, _) => true
    case _ => false
  }

  protected[ir] def join(ty1: Type, ty2: Type): Type =
    if (subtype(ty1, ty2))
      ty2
    else if (subtype(ty2, ty1))
      ty1
    else
      TAny

  protected[ir] def join(tys: Iterable[Type]): Type = tys.foldLeft[Type](TNothing)(join)

  protected[ir] def meet(ty1: Type, ty2: Type): Type =
    if (subtype(ty1, ty2))
      ty1
    else if (subtype(ty2, ty1))
      ty2
    else
      TNothing

  protected def meet(tys: Iterable[Type]): Type = tys.foldLeft[Type](TAny)(meet)

  protected[ir] def assertSubtype(ty1: Type, ty2: Type, location: SourceLocation*): Unit =
    if (!subtype(ty1, ty2))
      error(s"Expected $ty1 but got: $ty2", location: _*)

  protected[ir] def assignType(term: Typeable[Type] with SourceLocation)(computeType: => Type): Type =
    val inferred = computeType
    term.typ match
      case Some(annotated) =>
        if (!subtype(inferred, annotated))
          error(s"Inferred type $inferred, but expected annotated type $annotated", term)
        annotated
      case None =>
        term.typed(inferred)
        inferred

  protected[ir] def assertBound(term: Typeable[TermType] with SourceLocation): Unit =
    if (!term.typ.get.positive)
      error(s"Expected postively bound term, but got $term", term)
