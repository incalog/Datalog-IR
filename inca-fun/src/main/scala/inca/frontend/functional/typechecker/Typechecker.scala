package inca.frontend.functional.typechecker

import inca.frontend.functional.syntax.*
import inca.ir.Name
import inca.ir.typing.{Resolvable, TypeCastable}
import inca.ir.util.SourceLocation

class Typechecker extends TypeContext with TypeIO {

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

    bindVar(Name("min"), Var.BuiltInFunction, TFun(Seq(TInt, TInt), TInt))
    bindVar(Name("max"), Var.BuiltInFunction, TFun(Seq(TInt, TInt), TInt))
    bindVar(Name("abs"), Var.BuiltInFunction, TFun(Seq(TInt), TInt))
    bindVar(Name("toString"), Var.BuiltInFunction, TFun(Seq(TInt), TString))

    module.content.foreach {
      case fun: FunctionDef => // checked below
      case data: DataDef => typecheck(data)
    }

    module.content.foreach {
      case fun: FunctionDef => typecheck(fun)
      case data: DataDef => // check above
    }
  }

  private var currentFunctionDef: Option[FunctionDef] = None

  def typecheck(fun: FunctionDef): Unit = scopedTypeContext {
    fun.tyVars.foreach(tyVar => bindTyVar(tyVar.name, tyVar))
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
      val ty = typecheckExp(fun.body, fun.outType)
      if (!subtype(ty, fun.outType))
        error(s"Found body of type $ty, but expected function result type ${fun.outType}", fun.body)
    } finally {
      currentFunctionDef = oldFunctionDef
    }
  }

  def typecheck(data: DataDef): Unit = scopedTypeContext {
    data.tyVars.foreach(tyVar => bindTyVar(tyVar.name, tyVar))
    data.constrs.foreach { constr =>
      constr.paramTypes.foreach(typecheck)
      resolveTarget(constr)(data)
    }
  }

  def typecheck(typ: Type): Unit = typ match {
    case TTuple(tys) => tys.foreach(typecheck)
    case TSet(ty) => typecheck(ty)
    case TFun(from, to) =>
      from.foreach(typecheck)
      typecheck(to)
    case typ@TName(name) =>
      if (typ.isBuiltIn) {
        // ok
      } else if (isData(name)) {
        lookupData(name) match {
          case Some(data) => resolveTarget[TName.Target](typ)(data)
          case None => // nothing
        }
      } else if (isTypeVar(name)) {
        lookupTyVar(name) match {
          case Some(data) => resolveTarget[TName.Target](typ)(data)
          case None => // nothing
        }
      } else {
        error(s"Unresolved named type $typ", typ)
      }
    case typ@TApply(named@TName(name), tys) =>
      lookupData(name) match {
        case Some(data) =>
          resolveTarget[TName.Target](typ)(data)
          resolveTarget[TName.Target](named)(data)
          tys.foreach(typecheck)
        case None => // nothing
      }
    case TAny => // nothing
    case _ => throw new IllegalArgumentException(s"Currently does not support $typ")
  }

  /*
   * Expressions
   */

  final def typecheckExp(exp: Expression, anno: Type): Type = typecheckExp(exp, Some(anno))

  final def typecheckExp(exp: Expression, anno: Option[Type]): Type = assignType(exp, anno)(typecheckInternal(exp, anno))

  protected def typecheckInternal(exp: Expression, anno: Option[Type]): Type =
    typecheckCore(exp, anno)

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
    case let@Let(names, letanno, bound, body) =>
      val ty = typecheckExp(bound, letanno)
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
        typecheckExp(body, anno)
      }
    case If(cnd, thn, els) =>
      val cty = typecheckExp(cnd, TBoolean)
      if (!subtype(cty, TBoolean))
        error(s"Expected Boolean condition, but got $cty", cnd)
      val tty = typecheckExp(thn, anno)
      val ety = typecheckExp(els, anno)
      join(tty, ety)

    case Tuple(exps) =>
      val tys = anno match
        case Some(TTuple(ttys)) if ttys.size == exps.size =>
          exps.zip(ttys).map(p => typecheckExp(p._1, p._2))
        case _ => exps.map(typecheckExp(_, None))
      TTuple(tys)

    case lam@Lambda(vs, body) => scopedTypeContext {
      vs.foreach { case (v, ty) =>
        bindVar(v, lam, ty)
        typecheck(ty)
      }
      val ty = anno match
        case Some(TFun(_, tbody)) => typecheckExp(body, tbody)
        case _ => typecheckExp(body, None)
      TFun(vs.map(_._2), ty)
    }

    case Call(fun, tyArgs, args) =>
      val tfun = typecheckExp(fun, None)
      tfun match {
        case tfun: TFun =>
          typecheckFunDefCall(fun, tfun, tyArgs, args, exp)
        case _ =>
          error(s"Expression has type $tfun, but required function type", fun)
          tfun
      }

    case Match(matchee, cases) =>
      val matcheeTy = typecheckExp(matchee, None)
      matcheeTy match {
        case td: TName if isData(td.name) && td.target.isDefined =>
          typecheckTNameMatch(exp, cases, td, anno)
        case td: TName if isTypeVar(td.name) =>
          error(s"Cannot match on parametric type $td", matchee)
          val ctys = cases.map(c => typecheckExp(c._2, anno))
          join(ctys)
        case td: TApply if td.target.isDefined =>
          typecheckTConstrMatch(exp, cases, td, anno)
        case ty =>
          error(s"Cannot match on type $ty", matchee)
          val ctys = cases.map(c => typecheckExp(c._2, anno))
          join(ctys)
      }

    case IntLit(i) => TInt
    case DoubleLit(i) => TDouble
    case StringLit(i) => TString
    case BoolLit(i) => TBoolean

    case UnOp("-", e) =>
      val eTy = typecheckExp(e, anno)
      if (!subtype(eTy, TInt) && !subtype(eTy, TDouble))
        error(s"Required numeric type, but got $eTy", e)
      eTy
    case UnOp("!", e) =>
      val eTy = typecheckExp(e, anno)
      if (!subtype(eTy, TBoolean))
        error(s"Required boolean type, but got $eTy", e)
      eTy
    case BinOp(e1, op, e2) =>
      op match
        case "==" | "!=" =>
          val t1 = typecheckExp(e1, None)
          val t2 = typecheckExp(e2, Some(t1))
          if (meet(t1, t2) == TNothing)
            error(s"Incomparable expressions of type $t1 and $t2", exp)
          TBoolean
        case "+" =>
          val t1 = typecheckExp(e1, anno)
          val t2 = typecheckExp(e2, anno)
          if (subtype(t1, TString) && subtype(t2, TString))
            TString
          else if (subtype(t1, TInt) && subtype(t2, TInt))
            TInt
          else if (subtype(t2, TDouble) && subtype(t2, TDouble))
            TDouble
          else {
            error(s"Operator $op requires arguments of the same numeric or string type, but got $t1 and $t2", e1, e2)
            TDouble
          }
        case "-" | "*" | "/" =>
          val t1 = typecheckExp(e1, anno)
          val t2 = typecheckExp(e2, anno)
          if (subtype(t1, TInt) && subtype(t2, TInt))
            TInt
          else if (subtype(t2, TDouble) && subtype(t2, TDouble))
            TDouble
          else {
            error(s"Numeric operator $op requires arguments of the same type, but got $t1 and $t2", e1, e2)
            TDouble
          }
        case "%" =>
          val t1 = typecheckExp(e1, TInt)
          val t2 = typecheckExp(e2, TInt)
          if (subtype(t1, TInt) && subtype(t2, TInt))
            TInt
          else {
            error(s"Numeric operator $op requires Int arguments, but got $t1 and $t2", e1, e2)
            TInt
          }
        case ">" | ">=" | "<=" | "<" =>
          val t1 = typecheckExp(e1, None)
          val t2 = typecheckExp(e2, None)
          if (subtype(t1, TInt) && subtype(t2, TInt))
            TBoolean
          else if (subtype(t2, TDouble) && subtype(t2, TDouble))
            TBoolean
          else {
            error(s"Comparator $op requires arguments of the same type, but got $t1 and $t2", e1, e2)
            TBoolean
          }
        case "&&" | "||" =>
          val t1 = typecheckExp(e1, TBoolean)
          val t2 = typecheckExp(e2, TBoolean)
          if (subtype(t1, TBoolean) && subtype(t2, TBoolean))
            TBoolean
          else {
            error(s"Boolean operator $op requires Boolean arguments, but got $t1 and $t2", e1, e2)
            TInt
          }
        case "++" | "&" =>
          val t1 = typecheckExp(e1, anno)
          val t2 = typecheckExp(e2, anno)
          if (subtype(t1, TSet(TAny)) && subtype(t2, TSet(TAny))) {
            if (op == "++") join(t1, t2) else meet(t1, t2)
          } else {
            error(s"Set operator $op requires Set arguments, but got $t1 and $t2", e1, e2)
            TInt
          }
        case _ =>
          error(s"Unknown operator $op", exp)
          TAny

    case SetExp(es) =>
      val elemAnno = anno match
        case Some(TSet(ty)) => Some(ty)
        case _ => None
      val etys = es.map(typecheckExp(_, elemAnno))
      val joined = join(etys)
      TSet(joined)

    case mem: SetMember =>
      typecheckSetMember(mem, bindTupVars = false)
      TBoolean

    case SetComprehension(build, preds) => scopedTypeContext {
      // first check predicates and bind `x in S` variables
      preds.foreach {
        case mem: SetMember =>
          typecheckSetMember(mem, bindTupVars = true)

        case pred =>
          val tyPred = typecheckExp(pred, TBoolean)
          if (!subtype(tyPred, TBoolean))
            error(s"Comprehension predicate must have Boolean type, but got  $tyPred", pred)
      }

      // then check build and predicates
      val elemAnno = anno match
        case Some(TSet(ty)) => Some(ty)
        case _ => None
      val tyb = typecheckExp(build, elemAnno)
      TSet(tyb)
    }

    case SetFold(tyAnno, init, op, set) =>
      val tyInit = typecheckExp(init, anno)
      val tyOp = typecheckExp(op, None)
      val tySet = typecheckExp(set, None)
      val tySetContent = tySet match {
        case TSet(ty) => ty
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

    case _ => throw new UnsupportedOperationException(s"No type rule for $exp found.")
  }

  def typecheckSetMember(mem: SetMember, bindTupVars: Boolean): Unit = {
    val tySetContent = mem match {
      case SetMember(_, Var(name), _) if isData(name) =>
        // this is a type contains test
        mem.isTypeMember = true
        error(s"Type contains test is not supported currently", mem)
        TAny

      case SetMember(_, set, _) =>
        val tset = typecheckExp(set, None)
        tset match {
          case TSet(tsetContent) =>
            tsetContent
          case ty =>
            error(s"Required set type, but got $ty", set)
            TAny
        }
    }

    if (mem.neg || !bindTupVars) {
      val tyTup = typecheckExp(mem.tup, Some(tySetContent))
      if (!subtype(tyTup, tySetContent))
        error(s"Expected $tySetContent, but got $tyTup")
    } else mem.tup match {
      case v: Var if isFreeVar(v.name) =>
        bindVar(v.name, mem, tySetContent)
        assignType(v, None)(tySetContent)
      case Tuple(es) if tySetContent.isInstanceOf[TTuple] =>
        val tys = tySetContent.asInstanceOf[TTuple].ts
        if (tys.size != es.size)
          error(s"Set contains ${tys.size}-ary tuples, but test expression is ${es.size}-ary", mem)
        tys.zipAll(es, null, null).foreach {
          case (null, v@Var(x)) if isFreeVar(x) =>
            bindVar(x, mem, TAny)
            assignType(v, None)(TAny)
          case (null, e) =>
            typecheckExp(e, None)
          case (ty, null) =>
          case (ty, v@Var(x)) if isFreeVar(x) =>
            bindVar(x, mem, ty)
            assignType(v, None)(ty)
          case (ty, e) =>
            val tye = typecheckExp(e, Some(ty))
            if (!subtype(tye, ty))
              error(s"Expected $ty, but got $tye", e)
        }
      case _ =>
        val tyTup = typecheckExp(mem.tup, Some(tySetContent))
        if (!subtype(tyTup, tySetContent))
          error(s"Expected $tySetContent, but got $tyTup")
    }
  }

  private def typecheckTNameMatch(exp: Expression, cases: Seq[(Pattern, Expression)], td: TName, anno: Option[Type]): Type = {
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
            resolveTarget(pat)((dcon, data))
            if (paramTypes.size != vars.size)
              error(s"Wrong number of constructor arguments, expected ${paramTypes.size} but got ${vars.size}", pat)
            scopedTypeContext {
              vars.zipAll(paramTypes, null, null).foreach {
                case (null, ty) => // nothing
                case (v, null) => bindVar(v.name, pat, TAny)
                // TODO fix do type substitution for actual type
                case (v, ty) => bindVar(v.name, pat, ty)
              }
              typecheckExp(e, anno)
            }
          case None =>
            error(s"Cannot match constructor $constr against matchee of type $td", constr)
            scopedTypeContext {
              vars.foreach(v => bindVar(v.name, pat, TAny))
              typecheckExp(e, anno)
            }
        }
      case (pat, e) =>
        error(s"Cannot match pattern $pat against matchee of type $td", pat)
        scopedTypeContext {
          val dummy = ConstructorPattern(Name("?"), Seq())
          pat.vars.foreach(v => bindVar(v._1, dummy, TAny))
          typecheckExp(e, anno)
        }
    }
    val missingConstrs = data.constrs.map(_.name).toSet -- seenConstrs
    if (missingConstrs.nonEmpty)
      error(s"Pattern match must be complete but missed the following constructors: ${missingConstrs.mkString(", ")}", exp)
    join(ctys)
  }

  private def typecheckTConstrMatch(exp: Expression, cases: Seq[(Pattern, Expression)], td: TApply, anno: Option[Type]): Type = {
    val data = td.target.get.asInstanceOf[DataDef]
    var seenConstrs = Set[Name]()
    val subst = data.tyVars.map(v => TName(v.name)).zip(td.tys).toMap
    val substConstrs = data.constrs.map { constr =>
      DataConstructor(constr.name, constr.paramTypes.map(TypeUtil.substitute(_, subst)))
    }

    val ctys = cases.map {
      case (pat@ConstructorPattern(constr, vars), e) =>
        if (seenConstrs.contains(constr))
          error(s"Duplicate constructor pattern $constr", constr)
        else
          seenConstrs += constr

        substConstrs.find(_.name == constr) match {
          case Some(dcon@DataConstructor(_, paramTypes)) =>
            resolveTarget(pat)((dcon, data))
            if (paramTypes.size != vars.size)
              error(s"Wrong number of constructor arguments, expected ${paramTypes.size} but got ${vars.size}", pat)
            scopedTypeContext {
              vars.zipAll(paramTypes, null, null).foreach {
                case (null, ty) => // nothing
                case (v, null) => bindVar(v.name, pat, TAny)
                case (v, ty) => bindVar(v.name, pat, ty)
              }
              typecheckExp(e, anno)
            }
          case None =>
            error(s"Cannot match constructor $constr against matchee of type $td", constr)
            scopedTypeContext {
              vars.foreach(v => bindVar(v.name, pat, TAny))
              typecheckExp(e, anno)
            }
        }
      case (pat, e) =>
        error(s"Cannot match pattern $pat against matchee of type $td", pat)
        scopedTypeContext {
          val dummy = ConstructorPattern(Name("?"), Seq())
          pat.vars.foreach(v => bindVar(v._1, dummy, TAny))
          typecheckExp(e, anno)
        }
    }
    val missingConstrs = data.constrs.map(_.name).toSet -- seenConstrs
    if (missingConstrs.nonEmpty)
      error(s"Pattern match must be complete but missed the following constructors: ${missingConstrs.mkString(", ")}", exp)
    join(ctys)
  }

  def typecheckFunDefCall(fun: Expression, tfun: TFun, tyArgs: Seq[Type], args: Seq[Expression], exp: Expression): Type = {
    tyArgs.foreach(typecheck)
    if (tfun.from.size != args.size) {
      error(s"Function $fun expects ${tfun.from.size} arguments, but found ${args.size} arguments in call", exp)
    }

    // we need to do type substitution
    val substTFun = {
      val tyParams = typeParamsOfFunDef(fun)
      if (tyParams.size != tyArgs.size)
        error(s"Function $fun expects ${tyParams.size} type arguments, but found ${tyArgs.size} type arguments in call", exp)
      val subst = tyParams.map(param => TName(param.name)).zip(tyArgs).toMap
      TypeUtil.substitute(tfun, subst).asInstanceOf[TFun]
    }

    substTFun.from.zipAll(args, null, null) foreach {
      case (null, arg) =>
        typecheckExp(arg, None)
      case (param, null) =>
      // nothing
      case (tparam, arg) =>
        val argTy = typecheckExp(arg, tparam)
        val meetTy = meet(tparam, argTy)
        if (meetTy == TNothing) {
          error(s"Invalid argument of type $argTy for parameter of type $tparam", arg)
        }
    }

    val tres = substTFun.to
    typecheck(tres)
    tres
  }

  private def typeParamsOfFunDef(fun: Expression): Seq[ParametricType] = fun match {
    case funName@Var(_) =>
      funName.target.get match {
        case FunctionDef(_, _, _, typeParams, _, _, _) => typeParams
        case dc@DataConstructor(_, _) => dc.target.get match {
          case DataDef(_, _, _, tyVars, _) => tyVars
          case _ => Seq()
        }
        case _ => Seq()
      }
    case _ => Seq()
  }

  def subtype(ty1: Type, ty2: Type): Boolean =
    meet(ty1, ty2) == ty1

  protected def meet(tys: Iterable[Type]): Type =
    tys.foldLeft[Type](TAny)(meet)

  def meet(ty1: Type, ty2: Type): Type = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => ty1
    case (TAny, _) => ty2
    case (_, TAny) => ty1
    case (TName(name1), TApply(TName(name2), Seq())) if name1 == name2 => ty1
    case (TApply(TName(name1), Seq()), TName(name2)) if name1 == name2 => ty1
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size => TTuple(tys1.zip(tys2).map(tt => meet(tt._1, tt._2)))
    case (TSet(s1), TSet(s2)) => TSet(meet(s1, s2))
    case (TFun(fromTys1, toTy1), TFun(fromTys2, toTy2)) if fromTys1.size == fromTys2.size =>
      val argTys = fromTys1.zip(fromTys2).map { case (a1, a2) => join(a1, a2) } // contravariant in args
      val retTy = meet(toTy1, toTy2) // covariant in return
      TFun(argTys, retTy)
    case _ => TNothing
  }

  protected def join(tys: Iterable[Type]): Type =
    tys.foldLeft[Type](TNothing)(join)

  protected def join(ty1: Type, ty2: Type): Type = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => ty1
    case (TNothing, _) => ty2
    case (_, TNothing) => ty1
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size => TTuple(tys1.zip(tys2).map(tt => join(tt._1, tt._2)))
    case (TSet(s1), TSet(s2)) => TSet(join(s1, s2))
    case _ => TAny
  }

  def assignType(term: TypeCastable[Type] with SourceLocation, expected: Option[Type])(computeType: => Type): Type = {
    val inferred = computeType
    val result = term.typ match {
      case Some(annotated) =>
        if (!subtype(inferred, annotated))
          error(s"Inferred type $inferred, but expected annotated type $annotated", term)
        annotated
      case None =>
        term.typed(inferred)
        inferred
    }
    expected match
      case Some(ty) if ty != inferred =>
        term.casted(ty)
      case _ => // nothing
    result
  }

  def resolveTarget[T](term: Resolvable[T] with SourceLocation)(computeTarget: => T): T = {
    val newTarget = computeTarget
    term.resolved(newTarget, force = true)
    newTarget
  }

}