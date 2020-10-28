package inca.frontend.typechecker

import inca.frontend.core.Core._
import inca.frontend.parser.SourceLocation
import inca.frontend.util.TypeHelper
import inca.runtime.context.LanguageMetaInfo

import scala.meta.Term

class CoreTypechecker(lang: LanguageMetaInfo)
  extends TypeContext with TypeIO {


  def typecheck(program: Seq[Module]): Unit = scopedTypeContext {
    program.foreach(bindModule)
    program.foreach(typecheck)
  }

  /*
   * Module
   */

  def typecheck(module: Module): Unit = scopedTypeContext {
    for (name <- module.imports;
         imported <- lookupModule(name);
         fun <- imported.funs if !fun.vis.contains(Private)) {
      bindFun(fun, imported)
    }

    for (fun <- module.funs)
      bindFun(fun, module)

    module.funs.foreach(typecheck)
  }


  /*
   * Function
   */

  def typecheck(fun: PatternFunction): Unit = scopedTypeContext {
    fun.params.foreach(p => bindVar(p.name, p.typ))
    fun.bodies.foreach { body =>
      val ty = typecheck(body, requireTerminator = true)
      if (!TypeOps.subtype(ty, fun.outType, lang))
        error(s"Found body of type $ty, but expected function result type ${fun.outType}", body)
    }
  }

  def typecheck(body: Body, requireTerminator: Boolean): TypeAnno = scopedTypeContext {
    if (body.stmts.isEmpty)
      TUnit
    else {
      body.stmts.init.foreach { stm =>
        typecheck(stm) match {
          case NoTerminator => // fine, do nothing
          case Terminator(ty) =>
            warn(s"Found terminator statement in the middle of a block; subsequent statements are dead code.", stm)
        }
      }
      typecheck(body.stmts.last) match {
        case NoTerminator =>
          if (requireTerminator)
            warn(s"Block should end with a terminator statement", body)
          TUnit
        case Terminator(ty) => ty
      }
    }
  }

  /*
   * Statements
   */

  sealed trait StmType
  case object NoTerminator extends StmType
  case class Terminator(ty: TypeAnno) extends StmType

  def typecheck(stm: Statement): StmType = stm match {
    case core: CoreStatement => typecheckCore(core)
    case _ => throw new UnsupportedOperationException(s"No type rule for $stm found.")
  }

  final def typecheckCore(stm: CoreStatement): StmType = stm match {
    case Fail =>
      Terminator(TNothing)

    case Yield(exp) =>
      val ty = typecheck(exp)
      Terminator(ty)

    case Assert(cond) =>
      val condTy = typecheck(cond)
      if (condTy != TBool)
      error(s"Found condition of type $condTy, but expected $TBool", cond)
      NoTerminator

    case Values(name, typ) =>
      bindVar(name, typ)
      NoTerminator

    case Assign(names, exp) =>
      val ty = typecheck(exp)

      val namesStr = names.mkString("(", ", ", ")")
      ty match {
        case TUnit =>
          if (names.nonEmpty)
            error(s"Cannot assign expression of type $TUnit to $namesStr", stm)
          names.foreach(bindVar(_, TAny))
        case TTuple(tys) =>
          if (names.size != tys.size)
            error(s"Cannot assign ${tys.size}-ary tuple to $namesStr", stm)
          names.zipAll(tys, null, null).foreach {
            case (name, null) => bindVar(name, TAny)
            case (null, ty) => // nothing
            case (name, ty) => bindVar(name, ty)
          }
        case ty =>
          if (names.size != 1)
            error(s"Cannot assign expression of type $ty to $namesStr", stm)
          names.zipAll(Seq(ty), null, null).foreach {
            case (name, null) => bindVar(name, TAny)
            case (null, ty) => // nothing
            case (name, ty) => bindVar(name, ty)
          }
      }
      NoTerminator
  }


  /*
   * Expressions
   */

  final def typecheck(exp: Exp): TypeAnno = assignType(exp)(typecheck(exp, exp.typ))

  def typecheck(exp: Exp, anno: Option[TypeAnno]): TypeAnno = exp match {
    case core: CoreExp => typecheckCore(core, anno)
    case _ => throw new UnsupportedOperationException(s"No type rule for $exp found.")
  }

  final def typecheckCore(exp: CoreExp, anno: Option[TypeAnno]): TypeAnno = exp match {
    case Var(name) =>
      lookupVar(name).getOrElse(TAny)

    case Eq(lhs, rhs) =>
      val lty = typecheck(lhs)
      val rty = typecheck(rhs)
      if (TypeOps.meet(lty, rty, lang) == TNothing) {
        error(s"Cannot compare left-hand $lty with right-hand $rty", exp)
      }
      TBool

    case Neq(lhs, rhs) =>
      val lty = typecheck(lhs)
      val rty = typecheck(rhs)
      if (TypeOps.meet(lty, rty, lang) == TNothing) {
        error(s"Cannot compare left-hand $lty with right-hand $rty", exp)
      }
      TBool

    case InstanceOf(e, ty) =>
      val ety = typecheck(e)
      if (TypeOps.meet(ety, ty, lang) == TNothing) {
        warn(s"Cast of type $ety to unrelated type $ty will always fail", exp)
      }
      TBool

    case NotInstanceOf(e, ty) =>
      val ety = typecheck(e)
      if (TypeOps.meet(ety, ty, lang) == TNothing) {
        warn(s"Cast of type $ety to unrelated type $ty will always succeed", exp)
      }
      TBool

    case Def(e) =>
      if (!isValidDefUndefExp(e)) {
        error(s"Cannot test definedness of ${e.getClass.getName} expression", exp)
      }
      TBool

    case Undef(e) =>
      if (!isValidDefUndefExp(e)) {
        error(s"Cannot test definedness of ${e.getClass.getName} expression", exp)
      }
      TBool

    case Wildcard =>
      TAny

    case Constant(lit) =>
      lit match {
        case UnitLiteral => TUnit
        case BooleanLiteral(_) => TBool
        case IntLiteral(_) => TInt
        case LongLiteral(_) => TLong
        case DoubleLiteral(_) => TDouble
        case StringLiteral(_) => TString
      }

    case PathAccess(receiver, link) =>
      val rty = typecheck(receiver)
      typecheckLink(link, rty, exp)

    case Call(name, args, transitive) =>
      lookupFun(name) match {
        case None => TAny
        case Some(fun) => typecheckCall(fun, args, transitive, exp)
      }

    case Count(call) =>
      typecheck(call)
      TInt

    case Tuple(exps) =>
      exps.map(typecheck) match {
        case Seq() => TUnit
        case Seq(ty) => ty
        case tys => TTuple(tys)
      }

    case Eval(params, code) =>
      typecheckEval(params, code, exp)

    case Aggregate(init, join, unjoin, call) =>
      throw new UnsupportedOperationException(exp.toString)
  }



  def isValidDefUndefExp(exp: Exp): Boolean = exp match {
    case _: PathAccess => true
    case _: Call => true
    case _ => false
  }

  final def typecheckLink(link: Link, receiverTy: TypeAnno, exp: Exp): TypeAnno = link match {
    case NamedLink(field: Name) => receiverTy match {
      case TNode(node) =>
        lang.links.get(node, field.name) match {
          case Some(ty) => TypeOps.truechangeTypeToTypeAnno(ty)
          case _ => lang.litLinks.get(node, field.name) match {
            case Some(ty) => TypeOps.truechangeLitTypeToTypeAnno(ty)
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
        case TList(_) => TInt
        case _ =>
          error(s"Cannot access field `size` of non-list type in $receiverTy", exp)
          TInt
      }
  }

  def typecheckCall(fun: PatternFunction, args: Seq[Exp], transitive: Boolean, exp: Exp): TypeAnno = {
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
  def typecheckEval(params: Seq[Name], code: Term, exp: Exp): TypeAnno = {
    import scala.reflect.runtime.currentMirror
    import scala.tools.reflect.{ToolBox, ToolBoxError}

    // here we use a little hack. We create one big block that defines all the params with their type.
    // The initializing value is irrelevant.
    val paramString = params.flatMap { param =>
      lookupVar(param) match {
        case Some(ty) => Some(s"val $param: $ty = Predef.???")
        case None => None
      }
    }.mkString("; ")

    val codeSource = s"{$paramString; ${code.syntax}}"
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(codeSource)
    try {
      val typechecked = toolbox.typecheck(tree)
      val typ = typechecked.tpe.dealias
      TypeHelper.decode(typ.toString)
    } catch {
      case ToolBoxError(msg, _) =>
        error(msg, exp)
        TAny
    }
  }


  def assignType[T <: Typeable with SourceLocation](term: T)(computeType: => TypeAnno): TypeAnno = {
    val inferred = computeType
    term.typ match {
      case Some(annotated) =>
        if (!TypeOps.subtype(inferred, annotated, lang))
          error(s"Inferred type $inferred, but expected annotated type $annotated", term)
      case None => // nothing
    }
    term.typed(inferred)
    inferred
  }
}
