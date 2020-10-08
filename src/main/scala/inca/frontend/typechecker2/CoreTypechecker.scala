package inca.frontend.typechecker2

import inca.frontend.core.Core._
import inca.frontend.util.Program
import inca.runtime.context._

trait TypeCheckerExtension {
  def checkExp()(exp: Exp): TypeAnno
  def checkStatement()(stmt: Statement): Unit
}

class TypeChecker(extensions: Seq[TypeCheckerExtension])(languageMetaInfo: LanguageMetaInfo) {

  type FunEnv = Map[Name, PatternFunction]
  type ModuleEnv = Map[Name, Module]

  def checkProgram(prog: Program): Unit = {
    val moduleEnv = deriveModuleEnv(prog)
    prog.modules.foreach(checkModule(moduleEnv, _))
  }

  private def deriveModuleEnv(prog: Program): ModuleEnv = {
    val moduleEnv = prog.modules.map { m => m.name -> m }
    val moduleNames = moduleEnv.map(_._1)
    moduleNames.groupBy(identity).collect { case (x, List(_,_,_*)) =>
      throw new IllegalArgumentException(s"Module name $x is not unique")
    }
    moduleEnv.toMap
  }

  private def deriveFunEnv(modules: Seq[Module]): FunEnv = {
    val publicFuns = modules.flatMap { m =>
      m.funs.filter(_.vis.forall(_ == Public))
    }
    publicFuns.groupBy(identity).collect { case (x, List(_,_,_*)) =>
      throw new IllegalArgumentException(s"Function name $x is not unique")
    }
    publicFuns.map(f => f.name -> f).toMap
  }

  def checkModule(moduleEnv: ModuleEnv, module: Module): Unit = {
    val importedModules = module.imports.map(moduleEnv)
    val funEnv = deriveFunEnv(importedModules)
    val localFunEnv = module.funs.map(f => f.name -> f).toMap
    val finalFunEnv = funEnv ++ localFunEnv
    module.funs.foreach(checkFun(finalFunEnv))
  }

  type Ctx = Map[Name, TypeAnno]

  case class TypeContext(ctx: Ctx, funEnv: FunEnv, fun: PatternFunction) {

    def addBinding(name: Name, ty: TypeAnno): TypeContext = {
      if (ctx.contains(name))
        throw new IllegalArgumentException()
      TypeContext(ctx + (name -> ty), funEnv, fun)
    }

    def addBindings(bindings: Seq[(Name, TypeAnno)]): TypeContext = {
      val bindingExists = bindings.map(_._1).exists(ctx.keySet.contains)
      if (bindingExists)
        throw new IllegalArgumentException()
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

  def checkFun(funEnv: FunEnv)(fun: PatternFunction): Unit = {
    // create initial context
    val ctx = fun.params.map(p => p.name -> p.typ).toMap
    val typeCtx = TypeContext(ctx, funEnv, fun)
    fun.bodies.foreach(checkBody(typeCtx))
  }

  def checkBody(ctx: TypeContext)(body: Body): Unit =
    body.stmts.foldLeft(ctx) { case (newCtx, stmt) =>
      checkStatement(newCtx)(stmt)
    }

  def checkStatement(ctx: TypeContext)(stmt: Statement): TypeContext = stmt match {
    case Assign(names, exp) =>
      val ty = checkExp(ctx)(exp)
      if (names.isEmpty) {
        throw new IllegalArgumentException()
      } else if (names.size == 1) {
        ctx.addBinding(names.head, ty)
      } else {
        ty match {
          case TTuple(ts) =>
            if (ts.size == names.size) {
              ctx.addBindings(names.zip(ts))
            } else {
              throw new IllegalArgumentException
            }
          case _ => throw new IllegalArgumentException
        }
      }
    case Assert(exp) =>
      // TODO assert only works for eq, neq, inst, ninst, def, undef
      val ty = checkExp(ctx)(exp)
      val isBoolSubtype = TypeOps.subtype(ty, TBool, languageMetaInfo)
      if (isBoolSubtype) {
        throw new IllegalArgumentException("TODO implement")
      } else {
        throw new IllegalArgumentException()
      }
    case Values(name, ty) =>
      ctx.addBinding(name, ty)
    case Yield(exp) =>
      val ty = checkExp(ctx)(exp)
      val paramTys = ctx.fun.outParams.map(_.typ)
      ty match {
        case TTuple(tys) =>
          if (paramTys.size != tys.size)
            throw new IllegalArgumentException
          tys.zip(paramTys).foreach { case (ity, pty) =>
            val isSubtype = TypeOps.subtype(ity, pty, languageMetaInfo)
            if (!isSubtype)
              throw new IllegalArgumentException()
          }
          ctx
        case _ =>
          if (paramTys.size != 1) {
            throw new IllegalArgumentException()
          }
          val isSubtype = TypeOps.subtype(ty, paramTys.head, languageMetaInfo)
          if (!isSubtype)
            throw new IllegalArgumentException()
          ctx
      }
    case Fail => ctx
  }

  def checkExp(ctx: TypeContext)(exp: Exp): TypeAnno = exp match {
    case Var(name) =>
      ctx.ctx.get(name) match {
        case Some(ty) =>
          exp.typed(ty)
          ty
        case None => throw new IllegalArgumentException()
      }

    case Constant(lit) =>
      val litTy = (checkLit(lit))
      exp.typed(litTy)
      litTy

    case Wildcard =>
      // TODO Is it really TAny?
      val ty = TAny
      exp.typed(ty)
      ty

    case p@PathAccess(exp, link) =>
      val ty = checkExp(ctx)(exp)
      val trgTy = checkLink(link, ty)
      p.typed(trgTy)
      trgTy

    case Tuple(exps) =>
      val tys = exps.map(checkExp(ctx))
      val tupleTy = TTuple(tys)
      exp.typed(tupleTy)
      tupleTy

    case Call(name, args, _) =>
      val fun = ctx.funEnv.getOrElse(name, throw new IllegalArgumentException())

      val tys = args.map(checkExp(ctx))
      val paramTys = fun.params.map(_.typ)
      if (tys.size != paramTys.size)
        throw new IllegalArgumentException()
      tys.zip(paramTys).foreach { case (ty, pty) =>
        val isSubtype = TypeOps.subtype(ty, pty, languageMetaInfo)
        if(!isSubtype)
          throw new IllegalArgumentException()
      }

      val outTys = fun.outParams.map(_.typ)
      outTys match {
        case Nil =>
          //          throw new IllegalArgumentException()
          exp.typed(TUnit)
          TUnit
        case ty::Nil =>
          exp.typed(ty)
          ty
        case tys =>
          val tupleTy = TTuple(tys)
          exp.typed(tupleTy)
          tupleTy
      }

    case Count(call) =>
      checkExp(ctx)(call)
      exp.typed(TInt)
      TInt

    case eval@Eval(params, code) =>
      // TODO Do we want to annotate the return type of an eval or let the scala compiler figure it out?
      throw new IllegalArgumentException("TODO implement")

    // conditions
    case Def(exp) =>
      val ty = checkExp(ctx)(exp)
      exp.typed(ty)
      TBool
    case Undef(exp) =>
      val ty = checkExp(ctx)(exp)
      exp.typed(ty)
      TBool

    case Eq(lhs, rhs) =>
      val lhsty = checkExp(ctx)(lhs)
      val rhsty = checkExp(ctx)(rhs)
      if (TypeOps.meet(lhsty, rhsty, languageMetaInfo).isEmpty)
        throw new IllegalArgumentException()
      TBool

    case Neq(lhs, rhs) =>
      val lhsty = checkExp(ctx)(lhs)
      val rhsty = checkExp(ctx)(rhs)
      if (TypeOps.meet(lhsty, rhsty, languageMetaInfo).isEmpty)
        throw new IllegalArgumentException()
      TBool

    case instOf@InstanceOf(exp, ty) =>
      val expTy = checkExp(ctx)(exp)
      if (!(TypeOps.subtype(expTy, ty, languageMetaInfo) || expTy == ty))
        throw new IllegalArgumentException()
      instOf.typed(ty)
      ty

    case ninstOf@NotInstanceOf(exp, ty) =>
      val expTy = checkExp(ctx)(exp)
      if (!(TypeOps.subtype(expTy, ty, languageMetaInfo) || expTy == ty))
        throw new IllegalArgumentException()
      ninstOf.typed(ty)
      ty
  }

  def checkLit(lit: Literal): TypeAnno = lit match {
    case UnitLiteral => TUnit
    case BooleanLiteral(v) => TBool
    case IntLiteral(v) => TInt
    case LongLiteral(v) => TLong
    case DoubleLiteral(v) => TDouble
    case StringLiteral(v) => TString
  }

  def checkLink(link: Link, ty: TypeAnno): TypeAnno = link match {
    case NamedLink(field: Name) =>
      ty match {
        case TNode(node) =>
          languageMetaInfo.links.get(node, field) match {
            case Some(ty) => return TypeOps.truechangeTypeToTypeAnno(ty)
            case None => throw new IllegalArgumentException()
          }

          languageMetaInfo.litLinks.get(node, field) match {
            case Some(ty) => return TypeOps.truechangeLitTypeToTypeAnno(ty)
            case None => throw new IllegalArgumentException()
          }
        case _ => throw new IllegalArgumentException()
      }
    case ParentLink => TAny
    case ChildrenLink =>
      ty match {
        case TList(contained) => contained
        case _ => throw new IllegalArgumentException()
      }
      // TODO is it correct to return ty?
    case NextLink => ty
    case PreviousLink => ty
    case SizeLink =>
      ty match {
        case TList(_) => TInt
        case _ => throw new IllegalArgumentException()
      }
  }
}

