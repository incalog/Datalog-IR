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

  private var currentFunctionDef: Option[FunctionDef] = None
  def typecheck(fun: FunctionDef): Unit = scopedTypeContext {
    fun.params.foreach { p =>
      typecheck(p.typ)
      bindVar(p.name, p, p.typ)
    }
    val oldFunctionDef = currentFunctionDef
    try {
      currentFunctionDef = Some(fun)
      val ty = typecheck(fun.body).ty
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
    case _ =>
  }

  /*
   * Expressions
   */

  /**
   * Documents which functions have contributed to a value. We use this as part of a simple effect system.
   */
  type Origin = Set[FunctionDef]
  case class TypeOrigin(ty: Type, origin: Origin) {
    def ++(or2: Origin): TypeOrigin =
      TypeOrigin(ty, origin ++ or2)
  }
  object TypeOrigin {
    def unzip(it: Seq[TypeOrigin]): (Seq[Type], Origin) = {
      it.foldRight[(Seq[Type], Origin)]((Seq(),Set())) { case (TypeOrigin(ty, or), (tys, ors)) =>
        (ty +: tys, ors ++ or)
      }
    }
  }



  final def typecheck(exp: Expression): TypeOrigin = assignType(exp)(typecheckInternal(exp, exp.typ))

  protected def typecheckInternal(exp: Expression, anno: Option[Type]): TypeOrigin = exp match {
    case core: CoreExpression => typecheckCore(core, anno)
    case _ => throw new UnsupportedOperationException(s"No type rule for $exp found.")
  }

  final def typecheckCore(exp: CoreExpression, anno: Option[Type]): TypeOrigin = exp match {
    case v@Var(name) =>
      lookupVar(name) match {
        case Some((decl, ty)) =>
          resolveTarget(v)(decl)
          TypeOrigin(ty, Set())
        case None =>
          TypeOrigin(TAny, Set())
      }
    case let@Let(names, anno, bound, body) =>
      val TypeOrigin(ty, or) = typecheck(bound)
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
        typecheck(body) ++ or
      }

    case If(cnd, thn, els) =>
      val TypeOrigin(cty, or1) = typecheck(cnd)
      if (!subtype(cty, TScalaBoolean))
        error(s"Expected Boolean condition, but got $cty", cnd)
      val TypeOrigin(tty, or2) = typecheck(thn)
      val TypeOrigin(ety, or3) = typecheck(els)
      TypeOrigin(join(tty, ety), or1 ++ or2 ++ or3)

    case Tuple(exps) =>
      val (tys, ors) = TypeOrigin.unzip(exps.map(typecheck))
      TypeOrigin(TTuple(tys), ors)

    case call@Call(name, args, transitive) =>
      lookupCalled(name) match {
        case None => TypeOrigin(TAny, Set())
        case Some(Left(fun)) =>
          resolveTarget(call)(fun)
          typecheckFunCall(fun, args, transitive, exp) ++ Set(fun)
        case Some(Right((constr, data))) =>
          resolveTarget(call)(constr)
          typecheckConstrCall(constr, data, args, transitive, exp)
      }

    case Match(matchee, cases) =>
      typecheck(matchee) match {
        case TypeOrigin(td: TData, or) if td.target.isDefined =>
          typecheckTDataMatch(exp, cases, td) ++ or

        case TypeOrigin(topt: TOption, or) =>
          typecheckTOptionMatch(exp, matchee, cases, topt, or) ++ or

        case ty =>
          error(s"Cannot match on type $ty", matchee)
          val (ctys, ors) = TypeOrigin.unzip(cases.map(c => typecheck(c._2)))
          TypeOrigin(join(ctys), ors)
      }

    case BaseLit(code) =>
      TypeOrigin(typecheckDecodeScala(code.syntax, exp), Set())

    case BaseApply(fun, args) =>
      import meta._
      var ors: Origin = Set()
      val argTys = args.zipWithIndex.map { case (a, ix) =>
        val TypeOrigin(ty, or) = typecheck(a)
        ors ++= or
        ("param$_" + ix, ty)
      }
      val paramString = argTys.map { case (name, ty) =>
          Some(q"val ${Pat.Var(Term.Name(name))}: ${ty.asScala} = Predef.???".syntax)
      }.mkString(";\n")

      val codeSource = s"{$paramString;\n${fun.syntax}(..${argTys.map(a => Term.Name(a._1))})}"
      TypeOrigin(typecheckDecodeScala(codeSource, exp), ors)

    case BaseApplyInfix(left, op, right) =>
      import meta._
      val (leftName, TypeOrigin(leftTy, leftOr)) = (Term.Name("param$_left"), typecheck(left))
      val (rightName, TypeOrigin(rightTy, rightOr)) = (Term.Name("param$_right"), typecheck(right))

      (leftTy, op.tree.value, rightTy) match {
        case (TSet(tyl), "++",  TSet(tyr)) =>
          val ty = TSet(join(tyl, tyr))
          TypeOrigin(ty, leftOr ++ rightOr)
        case _ =>
          val paramString = Seq(
            q"val ${Pat.Var(leftName)}: ${leftTy.asScala} = Predef.???".syntax,
            q"val ${Pat.Var(rightName)}: ${rightTy.asScala} = Predef.???".syntax).mkString("\n")

          val codeSource = s"{$paramString;\n$leftName ${op.tree} $rightName}"
          TypeOrigin(typecheckDecodeScala(codeSource, exp), leftOr ++ rightOr)
      }


    case NoneExp() =>
      TypeOrigin(TOption(TNothing), Set())

    case SomeExp(e) =>
      val TypeOrigin(ty, or) = typecheck(e)
      TypeOrigin(TOption(ty), or)

    case SetExp(es) =>
      val (etys, ors) = TypeOrigin.unzip(es.map(typecheck))
      val joined = join(etys)
      TypeOrigin(TSet(joined), ors)

    case SetMember(tup, set) =>
      val TypeOrigin(tt, or1) =
        if (tup.size == 1)
          typecheck(tup.head)
        else
          typecheck(Tuple(tup))
      val TypeOrigin(tset, or2) = typecheck(set)
      tset match {
        case TSet(tsetContent) =>
          if (!subtype(tt, tsetContent))
            error(s"Expected $tsetContent, but got $tt")
        case ty =>
          error(s"Required set type, but got $ty", set)
      }
      TypeOrigin(TScalaBoolean, or1 ++ or2)

    case SetComprehension(build, preds) => scopedTypeContext {
      // first check predicates and bind `x in S` variables
      val ors = preds.map {
        case mem@SetMember(Seq(Var(x)), set) if isFreeVar(x) =>
          val TypeOrigin(ty, or) = typecheck(set)
          ty match {
            case TSet(tsetContent) => bindVar(x, mem, tsetContent)
            case _ => error(s"Required set type, but got $ty", set)
          }
          or
        case mem@SetMember(es, set) =>
          val TypeOrigin(ty, or) = typecheck(set)
          val ors = ty match {
            case TSet(TTuple(tys)) =>
              if (tys.size != es.size)
                error(s"Set contains ${tys.size}-ary tuples, but test expression is ${es.size}-ary", mem)
              val ors = tys.zipAll(es, null, null).map {
                case (null, Var(x)) if isFreeVar(x) =>
                  bindVar(x, mem, TAny)
                  Set()
                case (null, e) =>
                  typecheck(e).origin
                case (ty, null) =>
                  Set() // nothing
                case (ty, Var(x)) if isFreeVar(x) =>
                  bindVar(x, mem, ty)
                  Set()
                case (ty, e) =>
                  val TypeOrigin(tye, ore) = typecheck(e)
                  if (!subtype(tye, ty))
                    error(s"Expected $ty, but got $tye", e)
                  ore
              }
              ors.flatten
            case TSet(tsetContent) =>
              val TypeOrigin(tt, ortt) =
                if (es.size == 1)
                  typecheck(es.head)
                else
                  typecheck(Tuple(es))
              if (!subtype(tt, tsetContent))
                error(s"Expected $tsetContent, but got $tt")
              ortt
            case _ =>
              error(s"Required set type, but got $ty", set)
              val TypeOrigin(_, ortt) =
                if (es.size == 1)
                  typecheck(es.head)
                else
                  typecheck(Tuple(es))
              ortt
          }
          or ++ ors

        case pred =>
          val  TypeOrigin(tyPred, orPred) = typecheck(pred)
          if (!subtype(tyPred, TScalaBoolean))
            error(s"Comprehension predicate must have Boolean type, but got  $tyPred", pred)
          orPred
      }

      // then check build and predicates
      val  TypeOrigin(tyb, orb) = typecheck(build)
      TypeOrigin(TSet(tyb), orb ++ ors.flatten)
    }

  }

  private def typecheckTDataMatch(exp: CoreExpression, cases: Seq[(Pattern, Expression)], td: TData): TypeOrigin = {
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
    val (ctysTys, ors) = TypeOrigin.unzip(ctys)
    TypeOrigin(join(ctysTys), ors)
  }

  private def typecheckTOptionMatch(exp: CoreExpression, matchee: Expression, cases: Seq[(Pattern, Expression)], topt: TOption, or: Origin): TypeOrigin = {
    var seenConstrs = Set[String]()
    val ctys = cases.map {
      case (pat@NonePattern(), e) =>
        if (seenConstrs.contains("None"))
          error(s"Duplicate constructor pattern None", pat)
        else
          seenConstrs += "None"
        val tor = typecheck(e)
        val emptyBody = tor.ty match {
          case TNothing => true
          case TOption(TNothing) => true
          case TSet(TNothing) => true
          case _ => false
        }
        if (!emptyBody && currentFunctionDef.isDefined)
          ensureStratifiable(or, currentFunctionDef.get, exp)
        tor

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
    val (ctysTys, ors) = TypeOrigin.unzip(ctys)
    TypeOrigin(join(ctysTys), ors)
  }

  def typecheckFunCall(fun: FunctionDef, args: Seq[Expression], transitive: Boolean, exp: Expression): TypeOrigin = {
    val name = fun.name

    if (fun.params.size != args.size) {
      error(s"Function $name expects ${fun.params.size} arguments, but found ${args.size} arguments in call", exp)
    }

    var ors: Origin = Set()
    fun.params.zipAll(args, null, null) foreach {
      case (null, arg) =>
        typecheck(arg)
      case (param, null) =>
      // nothing
      case (param, arg) =>
        val TypeOrigin(argTy, or) = typecheck(arg)
        ors ++= or
        if (meet(param.typ, argTy) == TNothing) {
          warn(s"Cast of argument type $argTy to unrelated parameter type ${param.typ} will always fail", arg)
        }
    }

    if (transitive) {
      // TODO
    }

    TypeOrigin(fun.outType, ors)
  }

  def typecheckConstrCall(constr: DataConstructor, data: DataDef, args: Seq[Expression], transitive: Boolean, exp: Expression): TypeOrigin = {
    val name = constr.name

    if (constr.paramTypes.size != args.size) {
      error(s"Constructor $name expects ${constr.paramTypes.size} arguments, but found ${args.size} arguments in call", exp)
    }

    var ors: Origin = Set()
    constr.paramTypes.zipAll(args, null, null) foreach {
      case (null, arg) =>
        typecheck(arg)
      case (param, null) =>
      // nothing
      case (paramTy, arg) =>
        val TypeOrigin(argTy, or) = typecheck(arg)
        ors ++= or
        if (meet(paramTy, argTy) == TNothing) {
          warn(s"Cast of argument type $argTy to unrelated parameter type $paramTy will always fail", arg)
        }
    }

    if (transitive) {
      // TODO
    }

    TypeOrigin(TData(data.name).resolved(data), ors)
  }


  /**
   * The contributing functions may not call the containing function.
   * Only then can we negate a value derived from the contributing functions.
   */
  private def ensureStratifiable(contributingFunctions: Set[FunctionDef], containingFunction: FunctionDef, loc: SourceLocation): Unit = {
    var visited: Set[FunctionDef] = Set()
    var todoPaths: Seq[Seq[FunctionDef]] = contributingFunctions.toSeq.map(Seq(_))
    while (todoPaths.nonEmpty) {
      val path = todoPaths.head
      todoPaths = todoPaths.tail

      path.head.calls.foreach { call =>
        call.target match {
          case None =>
            // Calls with unresolved targets should already be marked as erroneous.
            warn(s"Unresoved call to ${call.name} prevented exact emptiness validity check.", call)
          case Some(_: DataConstructor) =>
            // Constructors cannot call functions, hence we can ignore them.
          case Some(fun: FunctionDef) =>
            if (visited.contains(fun)) {
              // skip
            } else if (fun == containingFunction) {
              val realPath = path.reverse
              error(
                s"""Invalid emptiness test.
                   |The matchee depends on function ${realPath.head.name}, which has a dependency chain
                   |  ${realPath.map(_.name).mkString("->")}->${containingFunction.name}
                   |to the containing function ${containingFunction.name}.""".stripMargin, loc)
              return
            } else {
              visited += fun
              todoPaths = (fun +: path) +: todoPaths
            }
        }
      }
    }
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

  def assignType(term: Typeable with SourceLocation)(computeType: => TypeOrigin): TypeOrigin = {
    val TypeOrigin(inferred, or) = computeType
    term.typ match {
      case Some(annotated) =>
        if (!subtype(inferred, annotated))
          error(s"Inferred type $inferred, but expected annotated type $annotated", term)
        TypeOrigin(annotated, or)
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
        TypeOrigin(resolved, or)
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