package inca.frontend.datalog.typechecker

import inca.compiler.source.SourceLocation
import inca.frontend.datalog.syntax._
import inca.frontend.util.{Resolvable, Typeable}
import inca.runtime.context.DataModel
import truechange.SortType

trait Typechecker extends TypeContext with TypeIO with ScalaTypeContext {

  val dataModel: DataModel

  /*
   * Module
   */

  def typecheck(module: Module): Unit = {
    // bind symbols first
    module.content.foreach {
      case sig: RuleSig => bindRelation(sig)
      case _: Rule => // nothing
      case data: DataDef => bindData(data)
    }

    module.content.foreach {
      case _: RuleSig => // nothing
      case rule: Rule => typecheck(rule)
      case data: DataDef => typecheck(data)
    }
  }

  def typecheck(data: DataDef): Unit =
    data.constrs.foreach(_.paramTypes.foreach(typecheck))

  def typecheck(typ: Type): Unit = typ match {
    case typ@TData(name) => lookupType(name) match {
      case Some(data) => resolveTarget[TData.Target](typ)(data)
      case None => // nothing
    }
    case _ =>
  }

  def typecheck(rule: Rule): Unit = scopedTypeContext {
    lookupRelation(rule.name) match {
      case Some(RuleSig(_, _, params)) =>
        if (params.size != rule.headTerms.size)
          error(s"Wrong number of terms in head, expected ${params.size} but got ${rule.headTerms.size}", rule)
        params.zipAll(rule.headTerms, null, null).foreach {
          case (null, term) =>
            typecheck(term, None)
          case (_, null) => // nothing
          case (Param(ty), term) =>
            typecheck(term, Some(ty))
        }
      case None =>
    }
    rule.body.foreach(typecheck)
  }

  def typecheck(atom: Atom): Unit = atom match {
    case Compare(_, lhs, rhs) =>
      val lty = typecheck(lhs, None)
      val rty = typecheck(rhs, None)
      if (meet(lty, rty) == TNothing) {
        error(s"Cannot compare left-hand $lty with right-hand $rty", atom)
      }
    case call@Call(name, args, not) => lookupCalled(name) match {
      case Some(called) => called match {
        case sig@RuleSig(annos, name, params) =>
          resolveTarget(call)(sig)
          if (params.size != args.size)
            error(s"Wrong number of arguments, expected ${params.size} but got ${args.size}", atom)
          params.zipAll(args, null, null).foreach {
            case (null, arg) =>
              typecheck(arg, None)
            case (_, null) => // nothing
            case (Param(ty), arg) =>
              typecheck(arg, Some(ty))
          }
        case constr@DataConstructor(name, params) =>
          resolveTarget(call)(constr)
          if (params.size + 1 != args.size)
            error(s"Wrong number of arguments, expected ${params.size + 1} but got ${args.size}", atom)
          val dataRef = TData(constr.name)
          resolveTarget(dataRef)(constr)
          val constrTypes = constr.paramTypes :+ dataRef
          constrTypes.zipAll(args, null, null).foreach {
            case (null, arg) =>
              typecheck(arg, None)
            case (_, null) => // nothing
            case (ty, arg) =>
              typecheck(arg, Some(ty))
          }
        case data@DataDef(annos, name, constrs) =>
          resolveTarget(call)(data)
          if (args.size != 1)
            error(s"Wrong number of arguments for type test $name, expected 1 but got ${args.size}", atom)
          args match {
            case Nil => // nothing
            case Seq(a) =>
              val td = TData(name)
              resolveTarget(td)(data)
              typecheck(a, Some(td))
            case a::as =>
              val td = TData(name)
              resolveTarget(td)(data)
              typecheck(a, Some(td))
              as.foreach(typecheck(_, None))
          }
      }
      case None => // nothing
    }
  }

  def typecheck(term: Term, anno: Option[Type]): Type = {
    val internal = typecheckInternal(term, anno)
    val ty = anno match {
      case None => internal
      case Some(annoTy) =>
        val met = meet(internal, annoTy)
        if (met == TNothing && internal != TNothing && annoTy != TNothing)
          error(s"Expected type $annoTy conflicts with inferred type $internal", term)
        met
    }
    assignType(term)(ty)
    ty
  }

  def typecheckInternal(term: Term, anno: Option[Type]): Type = term match {
      case Wildcard() =>
        anno.getOrElse(TAny)
      case Var(name) => lookupVar(name) match {
        case Some(ty) =>
          anno match {
            case None => ty
            case Some(annoTy) =>
              val met = meet(ty, annoTy)
              if (met == TNothing && ty != TNothing && annoTy != TNothing)
                error(s"Expected type $anno conflicts with inferred type $ty", term)
              bindVar(name, met)
              met
          }
        case None =>
          anno match {
            case Some(ty) =>
              bindVar(name, ty)
              ty
            case None =>
              error(s"Cannot infer type of $term", term)
              TAny
          }
      }
      case Constant(lit) => lit match {
        case IntLiteral(_) => TScalaInt
        case LongLiteral(_) => TScalaLong
        case DoubleLiteral(_) => TScalaDouble
        case StringLiteral(_) => TScalaString
        case BooleanLiteral(_) => TScalaBoolean
      }
      case p@Path(src, link) =>
        typecheck(src, None) match {
          case td: TData => td.target match {
            case Some(constr: DataConstructor) =>
              constr.params.find(_.name == link) match {
                case Some(cp@DataConstrParam(_, ty)) =>
                  resolveTarget(p)(cp)
                  ty
                case None =>
                  error(s"Constructor ${constr.name} does not have field $link", link)
                  TAny
              }
            case Some(data: DataDef) =>
              error(s"Cannot access fields of data type ${data.name}, you must first the constructor's type.", p)
              TAny
            case None =>
              TAny
          }
          case ty =>
            error(s"Cannot lookup field of $ty", link)
            TAny
        }
    }


  def subtype(ty1: Type, ty2: Type): Boolean =
    meet(ty1, ty2) == ty1

  protected def meet(tys: Iterable[Type]): Type =
    tys.foldLeft[Type](TAny)(meet)

  protected def meet(ty1: Type, ty2: Type): Type = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => ty1
    case (TAny, _) => ty2
    case (_, TAny) => ty1
    case (TData(n1), TData(n2)) =>
      if (dataModel.isSubtype(SortType(n1.name), SortType(n2.name)))
        ty1
      else if (dataModel.isSubtype(SortType(n2.name), SortType(n1.name)))
        ty2
      else
        TNothing
    case (TScala(s1), TScala(s2)) =>
      if (subtypeScala(s1.tree, s2.tree))
        ty1
      else if (subtypeScala(s2.tree, s1.tree))
        ty2
      else
        TNothing
    case (_, TScala(s2)) =>
      if (subtypeScala(ty1.asScala, s2.tree))
        ty1
      else
        TNothing
    case (TScala(s1), _) =>
      if (subtypeScala(s1.tree, ty2.asScala))
        ty2
      else
        TNothing
    case _ => TNothing
  }

  protected def join(tys: Iterable[Type]): Type =
    tys.foldLeft[Type](TNothing)(join)

  protected def join(ty1: Type, ty2: Type): Type = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => ty1
    case (TNothing, _) => ty2
    case (_, TNothing) => ty1
    case (TScala(s1), TScala(s2)) =>
      if (subtypeScala(s1.tree, s2.tree))
        ty2
      else if (subtypeScala(s2.tree, s1.tree))
        ty1
      else
        TAny
    case (_, TScala(s2)) =>
      if (subtypeScala(ty1.asScala, s2.tree))
        ty2
      else
        TAny
    case (TScala(s1), _) =>
      if (subtypeScala(s1.tree, ty2.asScala))
        ty1
      else
        TAny
    case _ => TAny
  }

  def assignType(term: Typeable[Type] with SourceLocation)(computeType: => Type): Type = {
    val inferred = computeType
    term.typ match {
      case Some(annotated) =>
        if (!subtype(inferred, annotated))
          error(s"Inferred type $inferred, but expected annotated type $annotated", term)
        annotated
      case None =>
        val resolved = inferred match {
          case td@TData(name) =>
            lookupType(name) match {
              case Some(data) =>
                resolveTarget(td)(data)
                inferred
              case None =>
                error(s"Could not find Data type $name", term)
                TAny
            }
          case _ => inferred
        }
        term.typed(resolved)
        resolved
    }
  }

  def resolveTarget[T](term: Resolvable[T] with SourceLocation)(computeTarget: => T): T = {
    val newTarget = computeTarget
    term.target match {
      case Some(oldTarget) =>
        if (oldTarget != newTarget)
          error(s"Resolved $term to new target $newTarget, which differs from previously computed target $oldTarget", term)
        oldTarget
      case None =>
        term.resolved(newTarget)
        newTarget
    }
  }

}
