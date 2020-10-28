package inca.frontend.typechecker3

import inca.frontend.core.Core._
import inca.frontend.parser.SourceLocation
import inca.frontend.util.TypeHelper
import inca.runtime.context.LanguageMetaInfo

import scala.meta.Term

class CoreTyper(lang: LanguageMetaInfo) 
  extends TypeContext with TypeIO {


  def assignType[T <: Typeable with SourceLocation](term: T)(computeType: => TypeAnno): TypeAnno = {
    val inferred = computeType
    term.typ match {
      case Some(annotated) =>
        if (!TypeOps.subtype(inferred, annotated, lang))
          error(s"Expected annotated type $annotated, but inferred $inferred, which is not a subtype of $annotated", term)
      case None => // nothing
    }
    term.typed(inferred)
    inferred
  }

  final def typecheck(exp: Exp): TypeAnno = assignType(exp)(typecheck(exp, exp.typ))

  def typecheck(exp: Exp, anno: Option[TypeAnno]): TypeAnno = exp match {
    case core: CoreExp => typecheckCore(core, anno)
    case _ => throw new UnsupportedOperationException(s"No type rule for $exp found.")
  }

  final def typecheckCore(exp: CoreExp, maybeAnno: Option[TypeAnno]): TypeAnno = exp match {
    case Var(name) =>
      lookupVar(name).getOrElse(TAny)

    case Eq(lhs, rhs) =>
      val lty = typecheck(lhs)
      val rty = typecheck(rhs)
      TypeOps.meet(lty, rty, lang).getOrElse {
        error(s"Cannot compare left-hand $lty with right-hand $rty", exp)
      }
      TBool

    case Neq(lhs, rhs) =>
      val lty = typecheck(lhs)
      val rty = typecheck(rhs)
      TypeOps.meet(lty, rty, lang).getOrElse {
        error(s"Cannot compare left-hand $lty with right-hand $rty", exp)
      }
      TBool

    case InstanceOf(e, ty) =>
      val ety = typecheck(e)
      TypeOps.meet(ety, ty, lang).getOrElse {
        warn(s"Cast of type $ety to unrelated type $ty will always fail", exp)
      }
      TBool

    case NotInstanceOf(e, ty) =>
      val ety = typecheck(e)
      TypeOps.meet(ety, ty, lang).getOrElse {
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
        TypeOps.meet(param.typ, argTy, lang).getOrElse {
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

}
