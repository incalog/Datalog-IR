package inca.frontend.typechecker

import inca.compiler.SourceLocation
import inca.frontend.core._

trait Typechecker extends TypeContext with TypeIO with ScalaTypeContext {

  def typecheck(program: Seq[Module]): Unit = scopedTypeContext {
    program.foreach(bindModule)
    program.foreach(typecheck)
  }

  /*
   * Module
   */

  def typecheck(module: Module): Unit = scopedTypeContext {
    for (imp <- module.imports;
         importedModule <- lookupModule(imp.name)) {
      resolveTarget(imp)(importedModule)

      for (content <- importedModule.content if !content.vis.contains(Private)) {
        content match {
          case fun: FunctionDef => bindFun(fun, importedModule)
          case data: DataDef => bindData(data, module)
        }
      }
    }

    // bind symbols first
    module.content.foreach {
      case fun: FunctionDef => bindFun(fun, module)
      case data: DataDef => bindData(data, module)
    }

    // type scala top-level definitions
    typecheckTopLevelObject()

    module.content.foreach {
      case fun: FunctionDef => typecheck(fun)
      case data: DataDef => typecheck(data)
    }
  }

  def typecheck(fun: FunctionDef): Unit = scopedTypeContext {
    fun.params.foreach { p =>
      typecheck(p.typ)
      bindVar(p.name, p, p.typ)
    }
    val ty = typecheck(fun.body)
    if (!subtype(ty, fun.outType))
      error(s"Found body of type $ty, but expected function result type ${fun.outType}", fun.body)
  }

  def typecheck(data: DataDef): Unit =
    data.constrs.foreach(_.paramTypes.foreach(typecheck))

  def typecheck(typ: Type): Unit = typ match {
    case typ@TData(name) => lookupData(name) match {
      case Some(data) => resolveTarget[TData.Target](typ)(data)
      case None => // nothing
    }
    case _ =>
  }

  /*
   * Expressions
   */

  final def typecheck(exp: Expression): Type = assignType(exp)(typecheckInternal(exp, exp.typ))

  protected def typecheckInternal(exp: Expression, anno: Option[Type]): Type = exp match {
    case core: CoreExpression => typecheckCore(core, anno)
    case _ => throw new UnsupportedOperationException(s"No type rule for $exp found.")
  }

  final def typecheckCore(exp: CoreExpression, anno: Option[Type]): Type = exp match {
    case v@Var(name) =>
      lookupVar(name) match {
        case Some((decl, ty)) =>
          resolveTarget(v)(decl)
          ty
        case None =>
          TAny
      }
    case let@Let(names, anno, bound, body) =>
      val ty = typecheck(bound)
      val namesStr = names.mkString("(", ", ", ")")
      scopedTypeContext {
        ty match {
          case TUnit =>
            if (names.nonEmpty)
              error(s"Cannot assign expression of type $TUnit to $namesStr", let)
            names.foreach(bindVar(_, let, TAny))
          case TTuple(tys) =>
            if (names.size != tys.size)
              error(s"Cannot assign ${tys.size}-ary tuple to $namesStr", let)
            names.zipAll(tys, null, null).foreach {
              case (name, null) => bindVar(name, let, TAny)
              case (null, ty) => // nothing
              case (name, ty) => bindVar(name, let, ty)
            }
          case ty =>
            if (names.size != 1)
              error(s"Cannot assign expression of type $ty to $namesStr", let)
            names.zipAll(Seq(ty), null, null).foreach {
              case (name, null) => bindVar(name, let, TAny)
              case (null, ty) => // nothing
              case (name, ty) => bindVar(name, let, ty)
            }
        }
        typecheck(body)
      }

    case If(cnd, thn, els) =>
      val cty = typecheck(cnd)
      if (!subtype(cty, TScalaBoolean))
        error(s"Expected Boolean condition, but got $cty", cnd)
      val tty = typecheck(thn)
      val ety = typecheck(els)
      meet(tty, ety)

    case Tuple(exps) =>
      TTuple(exps.map(typecheck))

    case call@Call(name, args, transitive) =>
      lookupCalled(name) match {
        case None => TAny
        case Some(Left(fun)) =>
          resolveTarget(call)(fun)
          typecheckFunCall(fun, args, transitive, exp)
        case Some(Right((constr, data))) =>
          resolveTarget(call)(constr)
          typecheckConstrCall(constr, data, args, transitive, exp)
      }

    case Match(matchee, cases) =>
      typecheck(matchee) match {
        case td: TData if td.target.isDefined =>
          val data = td.target.get.asInstanceOf[DataDef]
          var seenConstrs = Set[Name]()

          val ctys = cases.map { case (pat@ConstructorPattern(constr, vars), e) =>
            if (seenConstrs.contains(constr))
              error(s"Duplicate constructor pattern $constr", constr)
            else
              seenConstrs += constr

            data.constrs.find(_.name == constr) match {
              case Some(dcon@DataConstructor(_, paramTypes)) =>
                resolveTarget(pat)(dcon)
                if (paramTypes.size != vars.size)
                  error(s"Wrong number of constructor arguments, expected ${paramTypes.size} but got ${vars.size}", pat)
                scopedTypeContext {
                  vars.zipAll(paramTypes, null, null).foreach {
                    case (null, ty) => // nothing
                    case (v, null) => bindVar(v, pat, TAny)
                    case (v, ty) => bindVar(v, pat, ty)
                  }
                  typecheck(e)
                }
              case None =>
                error(s"Cannot match constructor $constr against matchee of type $td", constr)
                scopedTypeContext {
                  vars.foreach(v => bindVar(v, pat, TAny))
                  typecheck(e)
                }
            }
          }
          val missingConstrs = data.constrs.map(_.name).toSet -- seenConstrs
          if (missingConstrs.nonEmpty)
            error(s"Pattern match must be complete but missed the following constructors: ${missingConstrs.mkString(", ")}", exp)
          ctys.foldLeft[Type](TAny)((t1, t2) => meet(t1, t2))

        case ty =>
          error(s"Can only match on data types, but matchee has type $ty", matchee)
          val ctys = cases.map(c => typecheck(c._2))
          ctys.foldLeft[Type](TAny)((t1, t2) => meet(t1, t2))
      }


    case BaseLit(code) =>
      typecheckDecodeScala(code.syntax, exp)

    case BaseApply(fun, args) =>
      import meta._
      val argTys = args.zipWithIndex.map { case (a, ix) =>
        ("param$_" + ix, typecheck(a))
      }
      val paramString = argTys.map { case (name, ty) =>
          Some(q"val ${Pat.Var(Term.Name(name))}: ${ty.asScala} = Predef.???".syntax)
      }.mkString(";\n")

      val codeSource = s"{$paramString;\n${fun.syntax}(..${argTys.map(a => Term.Name(a._1))})}"
      typecheckDecodeScala(codeSource, exp)

    case BaseApplyInfix(left, op, right) =>
      import meta._
      val (leftName, leftTy) = (Term.Name("param$_left"), typecheck(left))
      val (rightName, rightTy) = (Term.Name("param$_right"), typecheck(right))
      val paramString = Seq(
        q"val ${Pat.Var(leftName)}: ${leftTy.asScala} = Predef.???".syntax,
        q"val ${Pat.Var(rightName)}: ${rightTy.asScala} = Predef.???".syntax).mkString("\n")

      val codeSource = s"{$paramString;\n$leftName ${op.tree} $rightName}"
      typecheckDecodeScala(codeSource, exp)
  }

  def typecheckFunCall(fun: FunctionDef, args: Seq[Expression], transitive: Boolean, exp: Expression): Type = {
    val name = fun.name

    if (fun.params.size != args.size) {
      error(s"Function $name expects ${fun.params.size} arguments, but found ${args.size} arguments in call", exp)
    }

    fun.params.zipAll(args, null, null) foreach {
      case (null, arg) =>
        typecheck(arg)
      case (param, null) =>
      // nothing
      case (param, arg) =>
        val argTy = typecheck(arg)
        if (meet(param.typ, argTy) == TNothing) {
          warn(s"Cast of argument type $argTy to unrelated parameter type ${param.typ} will always fail", arg)
        }
    }

    if (transitive) {
      // TODO
    }

    fun.outParams match {
      case Seq() => TUnit
      case Seq(out) => out
      case outs => TTuple(outs)
    }
  }

  def typecheckConstrCall(constr: DataConstructor, data: DataDef, args: Seq[Expression], transitive: Boolean, exp: Expression): Type = {
    val name = constr.name

    if (constr.paramTypes.size != args.size) {
      error(s"Constructor $name expects ${constr.paramTypes.size} arguments, but found ${args.size} arguments in call", exp)
    }

    constr.paramTypes.zipAll(args, null, null) foreach {
      case (null, arg) =>
        typecheck(arg)
      case (param, null) =>
      // nothing
      case (paramTy, arg) =>
        val argTy = typecheck(arg)
        if (meet(paramTy, argTy) == TNothing) {
          warn(s"Cast of argument type $argTy to unrelated parameter type $paramTy will always fail", arg)
        }
    }

    if (transitive) {
      // TODO
    }

    TData(data.name).resolved(data)
  }

  def typecheckDecodeScala(codeSource: String, loc: SourceLocation): Type = {
    typecheckScala(codeSource) match {
      case Left(typ) =>
        TypeHelper.decode(typ) match {
          case Right(ty) => ty
          case Left(msg) =>
            error(msg, loc)
            TAny
        }
      case Right(err) =>
        error(err.getMessage, loc)
        TAny
    }
  }
  def subtype(ty1: Type, ty2: Type): Boolean =
    meet(ty1, ty2) == ty1

  protected def meet(ty1: Type, ty2: Type): Type = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => ty1
    case (TAny, _) => ty2
    case (_, TAny) => ty1
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size => TTuple(tys1.zip(tys2).map(tt => meet(tt._1, tt._2)))
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

  def assignType(term: Typeable with SourceLocation)(computeType: => Type): Type = {
    val inferred = computeType
    term.typ match {
      case Some(annotated) =>
        if (!subtype(inferred, annotated))
          error(s"Inferred type $inferred, but expected annotated type $annotated", term)
        annotated
      case None =>
        val resolved = inferred match {
          case td@TData(name) =>
            lookupData(name) match {
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