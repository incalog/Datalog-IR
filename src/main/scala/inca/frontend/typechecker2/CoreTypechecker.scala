package inca.frontend.typechecker2

import inca.frontend.core.Core._
import inca.runtime.context._


class TypeError(msg: String) extends RuntimeException(msg)

object TypeError {
  def apply(msg: String): TypeError = new TypeError(msg)
}

case class TypeContext(ctx: VarCtx, funEnv: FunEnv, fun: PatternFunction) {

  // stmt is used for better error reporting
  def addBinding(name: Name, ty: TypeAnno, stmt: Statement): TypeContext = {
    if (ctx.contains(name))
      throw TypeError(s"$name is rebound in ${stmt.prettyprint("")}")
    TypeContext(ctx + (name -> ty), funEnv, fun)
  }

  // stmt is used for better error reporting
  def addBindings(bindings: Seq[(Name, TypeAnno)], stmt: Statement): TypeContext = {
    val binding = bindings.map(_._1).filter(ctx.keySet.contains)
    binding.foreach { name =>
      throw TypeError(s"$name is rebound in ${stmt.prettyprint("")}")
    }
    TypeContext(ctx ++ bindings, funEnv, fun)
  }

  // TODO need to think about binding refinement
  // look at type refinement type systems
  def refineBinding(name: Name, ty: TypeAnno): TypeContext = {
    val prevTy = ctx.get(name)
    prevTy match {
      case Some(value) =>
        TypeContext(ctx + (name -> ty), funEnv, fun)
      case None =>
        TypeContext(ctx + (name -> ty), funEnv, fun)
    }
  }
}


trait TypeCheckerExtension {
  def checkExp()(exp: Exp): TypeAnno
  def checkStatement()(stmt: Statement): Unit
}

class TypeChecker(extensions: Seq[TypeCheckerExtension])(languageMetaInfo: LanguageMetaInfo) {

  def checkModules(mods: Seq[Module]): Unit = {
    val moduleEnv = deriveModuleEnv(mods)
    mods.foreach(checkModule(moduleEnv, _))
  }

  private def deriveModuleEnv(mods: Seq[Module]): ModuleEnv = {
    val moduleEnv = mods.map { m => m.name -> m }
    val moduleNames = moduleEnv.map(_._1)
    moduleNames.groupBy(identity).collect { case (x, List(_,_,_*)) =>
      throw TypeError(s"Module name $x is not unique")
    }
    moduleEnv.toMap
  }

  private def deriveFunEnv(modules: Seq[Module]): FunEnv = {
    val publicFuns = modules.flatMap { m =>
      m.funs.filter(_.vis.forall(_ == Public))
    }
    publicFuns.groupBy(identity).collect { case (x, List(_,_,_*)) =>
      throw TypeError(s"Function name $x is not unique")
    }
    publicFuns.map(f => f.name -> f).toMap
  }

  def checkModule(moduleEnv: ModuleEnv, module: Module): Unit = {
    val importedModules = module.imports.map(moduleEnv)
    val funEnv = deriveFunEnv(importedModules)
    val localFunEnv = module.funs.map(f => f.name -> f).toMap
    val overlapping = localFunEnv.keySet.find(funEnv.keySet.contains)
    if (overlapping.isDefined)
      throw TypeError(s"Function name ${overlapping.get} is not unique")
    val finalFunEnv = funEnv ++ localFunEnv
    module.funs.foreach(checkFun(finalFunEnv))
  }



  def checkFun(funEnv: FunEnv)(fun: PatternFunction): Unit = {
    // create initial context
    val ctx = fun.params.map(p => p.name -> p.typ).toMap
    val typeCtx = TypeContext(ctx, funEnv, fun)
    fun.bodies.foreach(checkBody(typeCtx))
  }

  def checkBody(ctx: TypeContext)(body: Body): Unit = {
    val newCtx = body.stmts.init.foldLeft(ctx) { case (newCtx, stmt) =>
      checkStatement(newCtx)(stmt)
    }
    checkYield(newCtx)(body.stmts.last)
  }

  def checkStatement(ctx: TypeContext)(stmt: Statement): TypeContext = stmt match {
    case Assign(names, exp) =>
      val ty = checkExp(ctx)(exp)
      if (names.isEmpty) {
        throw TypeError(s"Cannot assign to empty variable list in ${stmt.prettyprint("")}")
      } else if (names.size == 1) {
        ctx.addBinding(names.head, ty, stmt)
      } else {
        ty match {
          case TTuple(ts) =>
            if (ts.size == names.size) {
              ctx.addBindings(names.zip(ts), stmt)
            } else {
              throw TypeError(s"Cannot assign ${ts.size}-ary tuple $ty to ${names.size} variables in ${stmt.prettyprint("")}")
            }
          case _ => throw TypeError(s"Cannot assign non-tuple $ty to variables $names in ${stmt.prettyprint("")}")
        }
      }

    case Assert(exp) =>
      val ty = checkExp(ctx)(exp)
      if (ty != TBool)
        throw TypeError(s"Cannot pass expression of type ${ty} to assert in ${stmt}, expected boolean typed expression")
      // TODO this is so hacky, is there a better way? this is the only case where we change a binding in the context
      // I want to avoid having an output context for expressions

      // refine ctx for variable
      exp match {
        case InstanceOf(Var(name), tyAnno) =>
          ctx.refineBinding(name, tyAnno)
        case InstanceOf(_, _) =>
          throw TypeError("TODO what should happen in this case?")
        case _ => ctx
      }

    case Fail => ctx

    case Values(name, ty) =>
      ctx.addBinding(name, ty, stmt)
    case Yield(_) => throw TypeError(s"Yield cannot be the non-last statement of a body")
  }

  def checkYield(ctx: TypeContext)(stmt: Statement): Unit = stmt match {
    case Yield(exp) =>
      val ty = checkExp(ctx)(exp)
      val paramTys = ctx.fun.outParams.map(_.typ)
      ty match {
        case TTuple(tys) =>
          if (paramTys.size != tys.size)
            throw TypeError(s"Cannot yield ${tys.size} values within function ${ctx.fun.name} which has ${paramTys.size} output parameters")
          tys.zip(paramTys).foreach { case (ity, pty) =>
            val isSubtype = TypeOps.subtype(ity, pty, languageMetaInfo)
            if (!isSubtype)
              throw TypeError(s"Cannot yield value of type ${ity.prettyprint} when function ${ctx.fun.name} expects ${pty.prettyprint}")
          }
        case _ =>
          if (paramTys.size != 1) {
            throw TypeError(s"Cannot yield single value within function ${ctx.fun.name} which has ${paramTys.size} output parameters")
          }
          val isSubtype = TypeOps.subtype(ty, paramTys.head, languageMetaInfo)
          if (!isSubtype)
            throw TypeError(s"Cannot yield type ${ty.prettyprint} when function ${ctx.fun.name} expects ${paramTys.head.prettyprint}")
      }
    // TODO Is this true?
    case _ => throw TypeError(s"Every body has to end with a yield statement, but got ${stmt.prettyprint("")}")
  }

  def typed(exp: Exp, ty: TypeAnno): TypeAnno = {
    exp.typed(ty)
    ty
  }

  // exp has annotated type after successful execution
  def checkExp(ctx: TypeContext)(exp: Exp): TypeAnno = exp match {
    case Var(name) =>
      ctx.ctx.get(name) match {
        case Some(ty) =>
          typed(exp, ty)
        case None => throw TypeError(s"Cannot access unbound name ${name}")
      }

    case Constant(lit) =>
      val litTy = (checkLit(lit))
      typed(exp, litTy)

    // TODO Is it really TAny?
    case Wildcard => typed(exp, TAny)

    case PathAccess(e, link) =>
      val ty = checkExp(ctx)(e)
      val trgTy = checkLink(link, ty, exp)
      typed(exp, trgTy)

    case Tuple(exps) =>
      val tys = exps.map(checkExp(ctx))
      val ty = TTuple(tys)
      typed(exp, ty)

    case Call(name, args, _) =>
      val fun = ctx.funEnv.getOrElse(
        name,
        throw TypeError(s"Function ${name} is not accessible in ${exp.prettyprint("")}"))

      val tys = args.map(checkExp(ctx))
      val paramTys = fun.params.map(_.typ)
      if (tys.size != paramTys.size)
        throw TypeError(s"Cannot pass ${tys.size} arguments to function ${name} in ${exp.prettyprint("")} which expects ${paramTys.size} arguments")
      tys.zip(paramTys).foreach { case (ty, pty) =>
        val isSubtype = TypeOps.subtype(ty, pty, languageMetaInfo)
        if(!isSubtype)
          throw TypeError(s"Cannot pass argument of type ${ty.prettyprint} where ${pty.prettyprint} is expected in ${exp.prettyprint("")}")
      }

      val outTys = fun.outParams.map(_.typ)
      outTys match {
        case Nil => typed(exp, TUnit)
        case ty::Nil => typed(exp, ty)
        case tys => typed(exp, TTuple(tys))
      }

    case Count(call) =>
      checkExp(ctx)(call)
      typed(exp, TInt)

    case eval@Eval(params, code) =>
      // TODO Do we want to annotate the return type of an eval or let the scala compiler figure it out?
      // I think we should anotate it and then let the scala compiler infer to check if correctly annotated
      throw new IllegalArgumentException("TODO implement")

    case Aggregate(init, join, unjoin, call) =>
      throw new IllegalArgumentException("TODO implement")

    // conditions
    case Def(e) => checkDefUndef(ctx)(exp, e)
    case Undef(e) => checkDefUndef(ctx)(exp, e)
      // TODO do we implicitly check that the number of variables are equal for assignments?
    case Eq(lhs, rhs) => checkComparing(ctx)(exp, lhs, rhs)
    case Neq(lhs, rhs) => checkComparing(ctx)(exp, lhs, rhs)
    case InstanceOf(e, ty) => checkInstanceOf(ctx)(exp, e, ty)
    case NotInstanceOf(e, ty) => checkInstanceOf(ctx)(exp, e, ty)
  }

  def checkComparing(ctx: TypeContext)(outer: Exp, lhs: Exp, rhs: Exp): TypeAnno = {
    val lhsty = checkExp(ctx)(lhs)
    val rhsty = checkExp(ctx)(rhs)
    if (TypeOps.meet(lhsty, rhsty, languageMetaInfo).isEmpty)
      throw TypeError(s"Cannot compare expressions of type ${lhsty.prettyprint} and ${rhsty.prettyprint} in ${outer.prettyprint("")}")
    typed(outer, TBool)
  }

  def checkDefUndef(ctx: TypeContext)(outer: Exp, e: Exp): TypeAnno = {
    validDefUndefArg(e)
    checkExp(ctx)(e)
    typed(outer, TBool)
  }

  def validDefUndefArg(exp: Exp): Unit = exp match {
    case PathAccess(_, _) => // do nothing
    case Call(_, _, _) => // do nothing
    case _ => throw TypeError(s"Cannot pass ${exp.prettyprint("")} to def/undef, expects path access or call")
  }

  def checkInstanceOf(ctx: TypeContext)(outer: Exp, e: Exp, ty: TypeAnno): TypeAnno = {
    val eTy = checkExp(ctx)(e)
    validInstanceOfArg(e, eTy)
    if (eTy != ty && !TypeOps.subtype(ty, eTy, languageMetaInfo))
      throw TypeError(s"Type ${ty.prettyprint} is not a subtype of ${eTy.prettyprint} in ${outer.prettyprint("")}")
    typed(outer, TBool)
  }

  def validInstanceOfArg(exp: Exp, ty: TypeAnno): Unit = ty match {
    case TTuple(_) => throw TypeError(s"Cannot pass ${exp.prettyprint("")} of tuple type to instanceOf/notInstanceOf")
    case _ => // do nothing and continue
  }

  def checkLit(lit: Literal): TypeAnno = lit match {
    case UnitLiteral => TUnit
    case BooleanLiteral(v) => TBool
    case IntLiteral(v) => TInt
    case LongLiteral(v) => TLong
    case DoubleLiteral(v) => TDouble
    case StringLiteral(v) => TString
  }

  // exp is only needed for good error messages
  def checkLink(link: Link, ty: TypeAnno, exp: Exp): TypeAnno = link match {
    case NamedLink(field: Name) =>
      ty match {
        case TNode(node) =>
          languageMetaInfo.links.get(node, field.name) match {
            case Some(ty) => return TypeOps.truechangeTypeToTypeAnno(ty)
            case _ => // do nothing
          }

          languageMetaInfo.litLinks.get(node, field.name) match {
            case Some(ty) => return TypeOps.truechangeLitTypeToTypeAnno(ty)
            case _ => // do nothing
          }
          throw TypeError(s"Cannot access link $field of node $node in ${exp.prettyprint("")}")
        case _ => throw TypeError(s"Cannot access named link of non-node type in ${exp.prettyprint("")}, expected node type but got ${ty.prettyprint}")
      }
    case ParentLink => TAny
    case ChildrenLink =>
      ty match {
        case TList(contained) => contained
        case _ => throw TypeError(s"Cannot access children link of non-list type in ${exp.prettyprint("")}, expected list type but got ${ty.prettyprint}")
      }
      // TODO is it correct to return ty?
    case NextLink => ty
    case PreviousLink => ty
    case SizeLink =>
      ty match {
        case TList(_) => TInt
        case _ => throw TypeError(s"Cannot access size link of non-list type in ${exp.prettyprint("")}, expected list type but got ${ty.prettyprint}")
      }
  }
}

