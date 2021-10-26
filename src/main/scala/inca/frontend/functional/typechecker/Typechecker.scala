package inca.frontend.functional.typechecker

import inca.compiler.SourceLocation
import inca.frontend.functional.core._
import inca.frontend.util.{Resolvable, Typeable}

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

  private var currentFunctionDef: Option[FunctionDef] = None

  def typecheck(fun: FunctionDef): Unit = scopedTypeContext {
    fun.params.foreach { p =>
      typecheck(p.typ)
      if (p.typ.isInstanceOf[TSet])
        error(s"Parameters may not range over relations", p)
      bindVar(p.name, p, p.typ)
    }
    typecheck(fun.outType)
    val oldFunctionDef = currentFunctionDef
    try {
      currentFunctionDef = Some(fun)
      val ty = typecheck(fun.body)
      if (!subtype(ty, fun.outType))
      error(s"Found body of type $ty, but expected function result type ${fun.outType}", fun.body)
    } finally {
      currentFunctionDef = oldFunctionDef
    }
  }

  def typecheck(data: DataDef): Unit =
    data.constrs.foreach(_.paramTypes.foreach(typecheck))

  def typecheck(typ: Type): Unit = typ match {
    case typ@TData(name) => lookupData(name) match {
      case Some(data) => resolveTarget[TData.Target](typ)(data)
      case None => // nothing
    }
    case TTuple(tys) => tys.foreach(typecheck)
    case TSet(ty) => typecheck(ty)
    case TOption(ty) => typecheck(ty)
    case TFun(from, to) =>
      from.foreach(typecheck)
      typecheck(to)
    case _ =>
  }

  /*
   * Expressions
   */

  final def typecheck(exp: Expression): Type = assignType(exp)(typecheckInternal(exp, exp.typ))

  protected def typecheckInternal(exp: Expression, anno: Option[Type]): Type = exp match {
    case core: Expression => typecheckCore(core, anno)
    case _ => throw new UnsupportedOperationException(s"No type rule for $exp found.")
  }

  final def typecheckCore(exp: Expression, anno: Option[Type]): Type = exp match {
    case v@Var(name) =>
      lookupVar(name) match {
        case Some((decl, ty)) =>
          if (ty.isInstanceOf[TSet])
            error(s"Variables may not range over relations", v)
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
    case TypeCast(e, ty) =>
      val ety = typecheck(e)
      if (meet(ety, ty) == TNothing)
        error(s"Type cast of ${ty} is not compatible with inferred type ${ety} of e", exp)
      ty
    case If(cnd, thn, els) =>
      val cty = typecheck(cnd)
      if (!subtype(cty, TScalaBoolean))
        error(s"Expected Boolean condition, but got $cty", cnd)
      val tty = typecheck(thn)
      val ety = typecheck(els)
      join(tty, ety)

    case Tuple(exps) =>
      val tys = exps.map(typecheck)
      TTuple(tys)

    case lam@Lambda(vs, body) => scopedTypeContext {
      vs.foreach { case (v, ty) =>
        bindVar(v, lam, ty)
        typecheck(ty)
      }
      val ty = typecheck(body)
      TFun(vs.map(_._2), ty)
    }

    case Call(fun, args, transitive) =>
      fun match {
        case Var(Name("parent")) =>
          if(args.size != 1)
            error(s"Built-in function parent expected one argument, but received ${args.size}", exp)
          val argTys = args.map(typecheck)
          argTys.headOption match {
            case Some(argTy) =>
              argTy match {
                case TData(name) =>
                  // do nothing
                case _ =>
                  error(s"Built-in function parent expected algebraic data type argument, but received argument of type $argTy", exp)
              }
            case None => // nothing
          }
          TOption(TAny)
        case _ =>
          val tfun = typecheck(fun)
          tfun match {
            case tfun: TFun =>
              typecheckFunDefCall(fun, tfun, args, transitive, exp)
            case _ =>
              error(s"Expression has type $tfun, but required function type", fun)
              tfun
          }
      }

    case Match(matchee, cases) =>
      typecheck(matchee) match {
        case td: TData if td.target.isDefined =>
          typecheckTDataMatch(exp, cases, td)

        case topt: TOption =>
          typecheckTOptionMatch(exp, matchee, cases, topt)

        case ty =>
          error(s"Cannot match on type $ty", matchee)
          val ctys = cases.map(c => typecheck(c._2))
          join(ctys)
      }

    case BaseLit(code) =>
      typecheckDecodeScala(code.syntax, exp)

    case BaseApply(fun, args) =>
      import meta._
      val argTys = args.zipWithIndex.map { case (a, ix) =>
        ("param$_" + ix, typecheck(a))
      }
      val paramString = argTys.map { case (name, ty) =>
          q"val ${Pat.Var(Term.Name(name))}: ${ty.asScala} = Predef.???".syntax
      }.mkString(";\n")

      val codeArgs = argTys.map(a => Term.Name(a._1))
      val codeSource = s"{$paramString;\n${fun.syntax}(${codeArgs.mkString(", ")})}"
      typecheckDecodeScala(codeSource, exp)

    case BaseApplyMethod(recv, method, args) =>
      import meta._
      val recvTy = typecheck(recv)
      val recvString = q"val ${Pat.Var(Term.Name(recv.prettyprint("")))}: ${recvTy.asScala} = Predef.???".syntax

      val codeSource =
        if (args.isEmpty) {
          s"{$recvString;\n${recv.prettyprint("")}.$method}"
        } else {
          val argTys = args.getOrElse(Seq()).zipWithIndex.map { case (a, ix) =>
            ("param$_" + ix, typecheck(a))
          }

          val paramString = argTys.map { case (name, ty) =>
            q"val ${Pat.Var(Term.Name(name))}: ${ty.asScala} = Predef.???".syntax
          }.mkString(";\n")
          val codeArgs = argTys.map(a => Term.Name(a._1))
          s"{$paramString;\n$recvString;\n${recv.prettyprint("")}.$method(${codeArgs.mkString(", ")})}"
        }
      typecheckDecodeScala(codeSource, exp)

    case BaseApplyInfix(left, op, right) =>
      import meta._
      val (leftName, leftTy) = (Term.Name("param$_left"), typecheck(left))
      val (rightName, rightTy) = (Term.Name("param$_right"), typecheck(right))

      (leftTy, op.tree.value, rightTy) match {
        case (TSet(tyl), "++",  TSet(tyr)) =>
          TSet(join(tyl, tyr))
        case (TSet(tyl), "&",  TSet(tyr)) =>
          TSet(join(tyl, tyr))
        case _ =>
          val paramString = Seq(
            q"val ${Pat.Var(leftName)}: ${leftTy.asScala} = Predef.???".syntax,
            q"val ${Pat.Var(rightName)}: ${rightTy.asScala} = Predef.???".syntax).mkString("\n")

          val codeSource = s"{$paramString;\n$leftName ${op.tree} $rightName}"
          typecheckDecodeScala(codeSource, exp)
      }


    case NoneExp() =>
      TOption(TNothing)

    case SomeExp(e) =>
      val ty = typecheck(e)
      TOption(ty)

    case SetExp(es) =>
      val etys = es.map(typecheck)
      val joined = join(etys)
      TSet(joined)

    case mem: SetMember =>
      typecheckSetMember(mem, bindTupVars = false)
      TScalaBoolean

    case SetComprehension(build, preds) => scopedTypeContext {
      // first check predicates and bind `x in S` variables
      preds.foreach {
        case mem: SetMember =>
          typecheckSetMember(mem, bindTupVars = true)

        case pred =>
          val tyPred = typecheck(pred)
          if (!subtype(tyPred, TScalaBoolean))
            error(s"Comprehension predicate must have Boolean type, but got  $tyPred", pred)
      }

      // then check build and predicates
      val tyb = typecheck(build)
      TSet(tyb)
    }

    case SetFold(tyAnno, init, op, set) =>
      val tyInit = typecheck(init)
      val tyOp = typecheck(op)
      val tySet = typecheck(set)
      val tySetContent = tySet match {
        case TSet(ty) => ty
        case TOption(ty) => ty
        case ty =>
          error(s"Can only fold over sets, but got $ty", set)
          TNothing
      }
      val tyFold = tyAnno.getOrElse(join(tyInit, tySetContent))
      tyOp match {
        case TFun(paramTypes, tyRes) =>
          if (paramTypes.size != 2 || !subtype(tyFold, paramTypes(0)) || !subtype(tyFold, paramTypes(1)) || !subtype(tyRes, tyFold))
            error(s"Expected function of type ($tyFold, $tyFold) => $tyFold, but $op has type $tyOp")
        case _ =>
          error(s"Expected function of type ($tyFold, $tyFold) => $tyFold, but $op has type $tyOp")
      }
      tyFold
  }

  def typecheckSetMember(mem: SetMember, bindTupVars: Boolean): Unit = {
    val tySetContent = mem match {
      case SetMember(_, Var(name), _) if isData(name) =>
        // this is a type member test
        mem.isTypeMember = true
        TData(name).resolved(lookupData(name).get)

      case SetMember(_, set, _) =>
        val tset = typecheck(set)
        tset match {
          case TSet(tsetContent) =>
            tsetContent
          case ty =>
            error(s"Required set type, but got $ty", set)
            TAny
        }
    }

    if (mem.neg || !bindTupVars) {
      val tyTup = typecheck(mem.tup)
      if (!subtype(tyTup, tySetContent))
        error(s"Expected $tySetContent, but got $tyTup")
    } else mem.tup match {
      case v: Var if isFreeVar(v.name) =>
        bindVar(v.name, mem, tySetContent)
        assignType(v)(tySetContent)
      case Tuple(es) if tySetContent.isInstanceOf[TTuple] =>
        val tys = tySetContent.asInstanceOf[TTuple].ts
        if (tys.size != es.size)
          error(s"Set contains ${tys.size}-ary tuples, but test expression is ${es.size}-ary", mem)
        tys.zipAll(es, null, null).foreach {
          case (null, v@Var(x)) if isFreeVar(x) =>
            bindVar(x, mem, TAny)
            assignType(v)(TAny)
          case (null, e) =>
            typecheck(e)
          case (ty, null) =>
          case (ty, v@Var(x)) if isFreeVar(x) =>
            bindVar(x, mem, ty)
            assignType(v)(ty)
          case (ty, e) =>
            val tye = typecheck(e)
            if (!subtype(tye, ty))
              error(s"Expected $ty, but got $tye", e)
        }
      case _ =>
        val tyTup = typecheck(mem.tup)
        if (!subtype(tyTup, tySetContent))
          error(s"Expected $tySetContent, but got $tyTup")
    }
  }

  private def typecheckTDataMatch(exp: Expression, cases: Seq[(Pattern, Expression)], td: TData): Type = {
    val data = td.target.get.asInstanceOf[DataDef]
    var seenConstrs = Set[Name]()

    val ctys = cases.map {
      case (pat@ConstructorPattern(constr, vars), e) =>
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
      case (pat, e) =>
        error(s"Cannot match pattern $pat against matchee of type $td", pat)
        scopedTypeContext {
          val dummy = ConstructorPattern(Name("?"), Seq())
          pat.vars.foreach(v => bindVar(v._1, dummy, TAny))
          typecheck(e)
        }
    }
    val missingConstrs = data.constrs.map(_.name).toSet -- seenConstrs
    if (missingConstrs.nonEmpty)
      error(s"Pattern match must be complete but missed the following constructors: ${missingConstrs.mkString(", ")}", exp)
    join(ctys)
  }

  private def typecheckTOptionMatch(exp: Expression, matchee: Expression, cases: Seq[(Pattern, Expression)], topt: TOption): Type = {
    var seenConstrs = Set[String]()
    val ctys = cases.map {
      case (pat@NonePattern(), e) =>
        if (seenConstrs.contains("None"))
          error(s"Duplicate constructor pattern None", pat)
        else
          seenConstrs += "None"
        typecheck(e)

      case (pat@SomePattern(v), e) =>
        if (seenConstrs.contains("Some"))
          error(s"Duplicate constructor pattern Some", pat)
        else
          seenConstrs += "Some"

        scopedTypeContext {
          bindVar(v, pat, topt.ty)
          typecheck(e)
        }

      case (pat@ConstructorPattern(constr, vars), e) =>
        error(s"Cannot match pattern $pat against matchee of type $topt", pat)
        scopedTypeContext {
          val dummy = ConstructorPattern(Name("?"), Seq())
          pat.vars.foreach(v => bindVar(v._1, dummy, TAny))
          typecheck(e)
        }
    }

    val missingConstrs = Set("None", "Some") -- seenConstrs
    if (missingConstrs.nonEmpty)
      error(s"Pattern match must be complete but missed the following constructors: ${missingConstrs.mkString(", ")}", exp)
    join(ctys)
  }

  def typecheckFunDefCall(fun: Expression, tfun: TFun, args: Seq[Expression], transitive: Boolean, exp: Expression): Type = {
    if (tfun.from.size != args.size) {
      error(s"Function $fun expects ${tfun.from.size} arguments, but found ${args.size} arguments in call", exp)
    }

    tfun.from.zipAll(args, null, null) foreach {
      case (null, arg) =>
        typecheck(arg)
      case (param, null) =>
      // nothing
      case (tparam, arg) =>
        val argTy = typecheck(arg)
        if (meet(tparam, argTy) == TNothing) {
          error(s"Invalid argument of type $argTy for parameter of type $tparam", arg)
        }
    }

    if (transitive) {
      // TODO
    }

    tfun.to
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

  protected def meet(tys: Iterable[Type]): Type =
    tys.foldLeft[Type](TAny)(meet)

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
    case (TOption(s1), TOption(s2)) => TOption(meet(s1, s2))
    case (TSet(s1), TSet(s2)) => TSet(meet(s1, s2))
    case _ => TNothing
  }

  protected def join(tys: Iterable[Type]): Type =
    tys.foldLeft[Type](TNothing)(join)

  protected def join(ty1: Type, ty2: Type): Type = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => ty1
    case (TNothing, _) => ty2
    case (_, TNothing) => ty1
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size => TTuple(tys1.zip(tys2).map(tt => join(tt._1, tt._2)))
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
    case (TOption(s1), TOption(s2)) => TOption(join(s1, s2))
    case (TSet(s1), TSet(s2)) => TSet(join(s1, s2))
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