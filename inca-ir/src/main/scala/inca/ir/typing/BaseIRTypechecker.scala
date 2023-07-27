package inca.ir.typing

import inca.ir.util.SourceLocation
import inca.ir.*
import inca.ir.extension.data.DataDefinition
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.Seq

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
  }

  def typecheck(moduleEntry: ModuleEntry): Unit = moduleEntry match {
      case relation: Relation => scopedTypeContext { typecheck(relation) }
      case _ => throw IllegalArgumentException(s"Can not typecheck unknown entry: $moduleEntry")
  }

  def typecheck(relation: Relation): Unit = {
    // bind parameters
    relation.params.foreach { param =>
      bindVar(param.name, param, param.ty)
    }
    relation.bodies.foreach(b => scopedTypeContext { typecheck(b) })
  }

  def typecheck(body: Body): Unit = body.atoms.foreach(typecheck)

  def typecheck(atom: Atom): Unit = {

    def typecheckCall(name: Name, args: Seq[Term]): Unit = {
      lookupModuleEntry(name) match
        case Some(Relation(_, params, _)) =>
          if (args.size != params.size) {
            error(s"Expected ${params.size} arguments but got: ${args.size}", atom)
          } else {
            val paramTys = params.map(_.ty)
            // Assign a type to each call parameter in case the variable is unbound
            args.zip(paramTys).foreach {
              case (v@Var(name), ty) if lookupVar(name).isEmpty => v.typed(ty)
              case v => // Nothing
            }
            val argTys = args.map(typecheck)
            args.zip(argTys).zip(paramTys).foreach { case ((a, aTy), pTy) =>
              assertSubtype(pTy, aTy, a, atom)
            }
          }
        case _ => error(s"Can not lookup module entry with name: $name", atom)
    }

    atom match {
      case Call(name, args) =>
        typecheckCall(name, args)
      case NegCall(name, args) =>
        typecheckCall(name, args)
      case ExtensionalCall(name, args) =>
        typecheckCall(name, args)
      case NegExtensionalCall(name, args) =>
        typecheckCall(name, args)
      case Eq(lhs, rhs) =>
        (lhs, rhs) match {
          case (v@Var(name), _) =>
            val rhsTy = typecheck(rhs)
            // assign the expected type for unbound vars
            if (lookupVar(name).isEmpty)
              lhs.typed(rhsTy)
            typecheck(lhs)
          case (_, v@Var(name)) =>
            val lhsTy = typecheck(lhs)
            // assign the expected type for unbound vars
            if (lookupVar(name).isEmpty)
              rhs.typed(lhsTy)
            typecheck(rhs)
          case _ =>
            assertSubtype(typecheck(lhs), typecheck(rhs))
        }
      case Neq(lhs, rhs) =>
        val lhsTy = typecheck(lhs)
        val rhsTy = typecheck(rhs)
        assertSubtype(lhsTy, rhsTy, atom)
      case _ =>
        throw IllegalStateException(s"Can not typecheck unknown atom: $atom")
    }
  }

  def typecheck(term: Term): Type = assignType(term)(typecheckInternal(term, term.typ))

  protected[ir] def typecheckInternal(term: Term, inferred: Option[Type]): Type = term match {
    case v@Var(name) => (lookupVar(name), inferred) match {
      case (Some((_, ty)), _) => // lookup was a bound variable or a param
        ty
      case (None, Some(ty)) => // inferred a type for an unbound variable
        bindVar(name, v, ty)
        ty
      case (None, None) => // unbound variable, but we could not infer a type
        error(s"Unbound variable $name", term)
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

  protected[ir] def assignType(term: Typeable[Type] with SourceLocation)(computeType: => Type): Type = {
    val inferred = computeType
    term.typ match {
      case Some(annotated) =>
        if (!subtype(inferred, annotated))
          error(s"Inferred type $inferred, but expected annotated type $annotated", term)
        annotated
      case None =>
        term.typed(inferred)
        inferred
    }
  }