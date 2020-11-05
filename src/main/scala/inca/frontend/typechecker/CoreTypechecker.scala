package inca.frontend.typechecker

import inca.frontend.core._
import inca.frontend.parser.SourceLocation
import inca.frontend.util.TypeHelper
import inca.runtime.aggregate.Aggregation
import inca.runtime.context.LanguageMetaInfo
import inca.util.Meta
import inca.util.Meta.Scala

trait CoreTypechecker
  extends TypeContext with TypeIO {

  val lang: LanguageMetaInfo

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
          case fun: PatternFunction => bindFun(fun, importedModule)
          case valDef: ValDef => bindVar(valDef.name, valDef, valDef.getType.get)
          case _: ScalaModuleContent => // nothing
        }
      }
    }

    // bind symbols first
    module.content.foreach {
      case fun: PatternFunction => bindFun(fun, module)
      case _: ValDef => // scoped to remainder of module, hence bind later
      case _: ScalaModuleContent => // nothing
    }

    module.content.foreach {
      case fun: PatternFunction => typecheck(fun)
      case vd: ValDef => typecheck(vd)
      case _: ScalaModuleContent => // nothing
    }
  }


  /*
   * Function
   */

  def typecheck(fun: PatternFunction): Unit = scopedTypeContext {
    fun.params.foreach(p => bindVar(p.name, p, p.typ))
    fun.bodies.foreach { body =>
      val ty = typecheck(body, mustYield = true)
      if (!TypeOps.subtype(ty.asType, fun.outType, lang))
        error(s"Found body of type $ty, but expected function result type ${fun.outType}", body)
    }
  }

  def typecheck(body: Body, mustYield: Boolean): StmType = scopedTypeContext {
    if (body.stmts.isEmpty) {
      if (mustYield) {
        warn(s"Block should end with a terminator statement", body)
        Yields(TUnit)
      } else {
        NoYield
      }
    }
    else {
      body.stmts.init.foreach { stm =>
        typecheck(stm, mustYield = false, mayYield = false)
      }
      typecheck(body.stmts.last, mustYield, mayYield = true)
    }
  }

  /*
   * ValDef
   */

  def typecheck(valDef: ValDef): Unit = {
    val inferred = typecheck(valDef.exp)
    valDef.typ match {
      case Some(annotated) =>
        if (!TypeOps.subtype(inferred, annotated, lang))
          error(s"Inferred type $inferred, but expected annotated type $annotated", valDef)
        bindVar(valDef.name, valDef, annotated)
      case None =>
        bindVar(valDef.name, valDef, inferred)
    }
  }

  /*
   * Statements
   */

  final def typecheck(stm: Statement, mustYield: Boolean, mayYield: Boolean): StmType = typecheckInternal(stm, mustYield) match {
    case ty: NoYield.type =>
      if (mustYield) {
        warn(s"Block should end with a terminator statement", stm)
        Yields(TUnit)
      } else {
        ty
      }
    case ty: Yields =>
      if (!mayYield) {
        warn(s"Unexpected terminator statement", stm)
        NoYield
      } else  {
        ty
      }
  }

  protected def typecheckInternal(stm: Statement, mustYield: Boolean): StmType = stm match {
    case core: CoreStatement => typecheckCore(core)
    case _ => throw new UnsupportedOperationException(s"No type rule for $stm found.")
  }

  final def typecheckCore(stm: CoreStatement): StmType = stm match {
    case FailStatement =>
      Yields(TNothing)

    case Yield(exp) =>
      val ty = typecheck(exp)
      Yields(ty)

    case Assert(cond) =>
      val condTy = typecheck(cond)
      if (!TypeOps.subtype(condTy, TScalaBoolean, lang))
        error(s"Expected Boolean condition, but got $condTy", cond)
      NoYield

    case vals@Values(name, typ) =>
      bindVar(name, vals, typ)
      NoYield

    case as@Assign(names, exp) =>
      val ty = typecheck(exp)

      val namesStr = names.mkString("(", ", ", ")")
      ty match {
        case TUnit =>
          if (names.nonEmpty)
            error(s"Cannot assign expression of type $TUnit to $namesStr", stm)
          names.foreach(bindVar(_, as, TAny))
        case TTuple(tys) =>
          if (names.size != tys.size)
            error(s"Cannot assign ${tys.size}-ary tuple to $namesStr", stm)
          names.zipAll(tys, null, null).foreach {
            case (name, null) => bindVar(name, as, TAny)
            case (null, ty) => // nothing
            case (name, ty) => bindVar(name, as, ty.unroll)
          }
        case ty =>
          if (names.size != 1)
            error(s"Cannot assign expression of type $ty to $namesStr", stm)
          names.zipAll(Seq(ty), null, null).foreach {
            case (name, null) => bindVar(name, as, TAny)
            case (null, ty) => // nothing
            case (name, ty) => bindVar(name, as, ty.unroll)
          }
      }
      NoYield
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

    case Eq(lhs, rhs) =>
      val lty = typecheck(lhs)
      val rty = typecheck(rhs)
      if (TypeOps.meet(lty, rty, lang) == TNothing) {
        error(s"Cannot compare left-hand $lty with right-hand $rty", exp)
      }
      TScalaBoolean

    case Neq(lhs, rhs) =>
      val lty = typecheck(lhs)
      val rty = typecheck(rhs)
      if (TypeOps.meet(lty, rty, lang) == TNothing) {
        error(s"Cannot compare left-hand $lty with right-hand $rty", exp)
      }
      TScalaBoolean

    case InstanceOf(e, ty) =>
      val ety = typecheck(e)
      if (TypeOps.meet(ety, ty, lang) == TNothing) {
        warn(s"Test of type $ety to unrelated type $ty will always fail", exp)
      }
      TScalaBoolean

    case NotInstanceOf(e, ty) =>
      val ety = typecheck(e)
      if (TypeOps.meet(ety, ty, lang) == TNothing) {
        warn(s"Test of type $ety to unrelated type $ty will always succeed", exp)
      }
      TScalaBoolean

    case Cast(src, targetTyp) =>
      val ety = typecheck(src)
      if (TypeOps.meet(ety, targetTyp, lang) == TNothing) {
        warn(s"Cast of type $ety to unrelated type $targetTyp will always fail", exp)
      }
      targetTyp

    case Def(e) =>
      typecheck(e)
      if (!isValidDefUndefExp(e)) {
        error(s"Cannot test definedness of ${e.getClass.getName} expression", exp)
      }
      TScalaBoolean

    case Undef(e) =>
      typecheck(e)
      if (!isValidDefUndefExp(e)) {
        error(s"Cannot test definedness of ${e.getClass.getName} expression", exp)
      }
      TScalaBoolean

    case Wildcard =>
      TAny

    case Constant(lit) =>
      typecheckLiteral(lit)

    case PathAccess(receiver, link) =>
      val rty = typecheck(receiver)
      typecheckLink(link, rty, exp)

    case call@Call(name, args, transitive) =>
      lookupFun(name) match {
        case None =>
          args.foreach(typecheck)
          TAny
        case Some(fun) =>
          resolveTarget(call)(fun)
          typecheckCall(fun, args, transitive, exp)
      }

    case Count(call) =>
      typecheck(call)
      TScalaInt

    case Tuple(exps) =>
      exps.map(typecheck) match {
        case Seq() => TUnit
        case Seq(ty) => ty
        case tys => TTuple(tys)
      }

    case Eval(params, code) =>
      typecheckEval(params, code, exp)

    case Aggregate(agg, bodies) =>
      val aggTy = typecheck(agg)

      val bodiesTy = bodies.foldLeft[Type](TAny) { (bodiesTy, body) =>
        val Yields(ty) = typecheck(body, mustYield = true)
        TypeOps.meet(bodiesTy, ty, lang)
      }

      val bodiesScalaTy = bodiesTy.asScala
      val requiredAggTy = TScala(Scala(meta.Type.Apply(Meta.typeOf[Aggregation[_]], List(bodiesScalaTy))))

      if (!TypeOps.subtype(aggTy, requiredAggTy, lang))
        error(s"Expected $requiredAggTy, but got $aggTy", agg)

      TScala(Scala(bodiesScalaTy))
  }


  def isValidDefUndefExp(exp: Expression): Boolean = exp match {
    case _: PathAccess => true
    case _: Call => true
    case _ => false
  }

  def typecheckLiteral(lit: Literal): Type = lit match {
    case UnitLiteral => TUnit
    case BooleanLiteral(_) => TScalaBoolean
    case IntLiteral(_) => TScalaInt
    case LongLiteral(_) => TScalaLong
    case DoubleLiteral(_) => TScalaDouble
    case StringLiteral(_) => TScalaString
  }

  final def typecheckLink(link: Link, receiverTy: Type, exp: Expression): Type = link match {
    case NamedLink(field: Name) => receiverTy match {
      case TNode(node) =>
        lang.links.get(node, field.name) match {
          case Some(ty) => TypeOps.truechangeTypeToType(ty)
          case _ => lang.litLinks.get(node, field.name) match {
            case Some(ty) => TypeOps.truechangeLitTypeToType(ty)
            case _ =>
              error(s"Cannot access field `$field` of node $node", exp)
              TAny
          }
        }
      case _ =>
        error(s"Cannot access field `$field` of non-node type $receiverTy", exp)
        TAny
    }
    case ParentLink => TAny
    case ChildrenLink =>
      receiverTy match {
        case TList(contained) => contained
        case TNode(_) => TAny
        case _ =>
          error(s"Cannot access field `children` of type $receiverTy", exp)
          TAny
      }
    case NextLink => TAny
    case PreviousLink => TAny
    case SizeLink =>
      receiverTy match {
        case TList(_) => TScalaInt
        case _ =>
          error(s"Cannot access field `size` of non-list type in $receiverTy", exp)
          TScalaInt
      }
  }

  def typecheckCall(fun: PatternFunction, args: Seq[Expression], transitive: Boolean, exp: Expression): Type = {
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
        if (TypeOps.meet(param.typ, argTy, lang) == TNothing) {
          warn(s"Cast of argument type $argTy to unrelated parameter type ${param.typ} will always fail", arg)
        }
    }

    if (transitive) {
      // TODO
    }

    fun.outParams match {
      case Seq() => TUnit
      case Seq(out) => out.typ
      case outs => TTuple(outs.map(_.typ))
    }
  }

  /**
   * Computes the result type of an Eval expression and validates the contained Scala code for type correctness
   */
  def typecheckEval(params: Seq[EvalParam], code: Scala[meta.Term], exp: Expression): Type = {
    // here we use a little hack. We create one big block that defines all the params with their type.
    // The initializing value is irrelevant.
    val paramString = params.flatMap { param =>
      lookupVar(param.name) match {
        case Some((decl,ty)) =>
          resolveTarget(param)(decl)
          assignType(param)(ty)
          import meta._
          Some(q"val ${Pat.Var(Term.Name(param.name.name))}: ${ty.asScala} = Predef.???".syntax)
        case None => None
      }
    }.mkString(";\n")

    val codeSource = s"{$paramString;\n${code.syntax}}"

    Meta.typecheckScala(codeSource) match {
      case Left(typ) =>
        TypeHelper.decode(typ) match {
          case Right(ty) => ty
          case Left(msg) =>
            error(msg, exp)
            TAny
        }
      case Right(err) =>
        error(err.getMessage, exp)
        TAny
    }
  }


  def assignType(term: Typeable with SourceLocation)(computeType: => Type): Type = {
    val inferred = computeType
    term.typ match {
      case Some(annotated) =>
        if (!TypeOps.subtype(inferred, annotated, lang))
          error(s"Inferred type $inferred, but expected annotated type $annotated", term)
        annotated
      case None =>
        term.typed(inferred)
        inferred
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
