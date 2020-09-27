package inca.frontend.typechecker

import inca.frontend.core.Core._
import inca.frontend.parser.CoreParser
import inca.frontend.typechecker.CoreTypechecker.TypeEnvironment
import inca.frontend.util.{EvalHelper, Program, ScalaTypeError, TypeHelper}
import inca.runtime.context._
import truechange.{AnyType, ListType, SortType, Type}

import scala.collection.mutable.ArrayBuffer

/* The Typechecker results */
sealed trait TypecheckResult
case class SuccessTypecheck(warnings: Seq[TypeWarning]) extends TypecheckResult
case class FailTypecheck(errors: Seq[TypeError], warnings: Seq[TypeWarning])
    extends TypecheckResult

/* Errors that can occur in typechecking */
case class TypeWarning(msg: String)
case class TypeError(msg: String)
class FatalError(err: TypeError) extends Exception(err.msg)

object TypeError {

  def expected(exp: TypeAnno, actual: TypeAnno, prefix: String = "")(implicit ctx: TypeContext): TypeError =
    template(prefix, s"Expected ${exp.prettyprint}, but got ${actual.prettyprint}")

  def incompatibleReturnTypes(prefix: String = "")(implicit ctx: TypeContext): TypeError =
    template(prefix, "The bodies have incompatible return types")

  def tupleUnpack(lhs: TTuple, rhsLen: Int, prefix: String = "")(implicit ctx: TypeContext): TypeError =
    template(prefix, s"Cannot unpack tuple with ${lhs.ts.length} to tuple with $rhsLen members.")

  def sizeMismatch(exp: Int, act: Int, prefix: String = "")(implicit ctx: TypeContext): TypeError =
    template(prefix, s"Size mismatch. Expected $exp arguments, but got $act")

  def undefined(typ: String, name: String, prefix: String = "")(implicit ctx: TypeContext): TypeError =
    template(prefix, s"$typ $name does not exist")

  def expectedExp(exp: Seq[String], act: Exp, prefix: String = "")(implicit ctx: TypeContext): TypeError =
    template(prefix, s"Expected ${exp.mkString("(", " or ", ")")}, but found $act")

  def unrelated(t1: TypeAnno, t2: TypeAnno, prefix: String = "")(implicit ctx: TypeContext): TypeError =
    template(prefix, s"types $t1 and $t2 are unrelated")

  def incompleteStatement(stm: Statement, prefix: String = "")(implicit ctx: TypeContext): TypeError =
    template(prefix, s"Incomplete ${stm.getClass.getName} at the end of a function")

  def alreadyUsed(name: String, prefix: String = "")(implicit ctx: TypeContext): TypeError =
    template(prefix, s"Variable $name already used")

  private def template(prefix: String, msg: String)(implicit ctx: TypeContext) =
    TypeError(s"(${ctx.where}, $prefix):\n $msg")
}

/** TypeContext */
class TypeContext(
    val fname: String, // Name of the function (For error messages)
    val functions: Map[Name, PatternFunction], // Functions accessable from the module
    val module: Module, // The module the function is in
    val tenv: CoreTypechecker.TypeEnvironment = Map(), // The variable type context
) {
  def this(tc: TypeContext, tenv: TypeEnvironment) {
    this(tc.fname, tc.functions, tc.module, tenv)
  }

  def where: String = s"Function: $fname, Module: ${module.name}"
}

/* Companion object to Typechecker */
object CoreTypechecker {
  type TypeEnvironment = Map[String, TypeAnno]
}

/** IncA Typechecker
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  *
  * @version 0.0.0
  *
  * @param   lmi        LanguageMetaInfo (Depends on the language used).
  * @param   prog       The actual IncA program.
  * @param   extensions List of extensions in use.
  */
class CoreTypechecker(
    val lmi: LanguageMetaInfo,
    val prog: Program,
    extensions: Seq[TypecheckerExtension]
) {

  // Data //
  val warnings: ArrayBuffer[TypeWarning] = ArrayBuffer()
  val errors: ArrayBuffer[TypeError] = ArrayBuffer()

  val function_env: Map[Name, Map[Name, PatternFunction]] =
    prog.modules.map(m => m.name -> m.funs.map(f => f.name -> f).toMap).toMap

  extensions.foreach(_.typechecker = this)

  def addError(err: TypeError)(implicit ctx: TypeContext): Unit = {
    errors.addOne(err)
  }

  def addWarning(msg: String)(implicit ctx: TypeContext): Unit = {
    warnings.addOne(TypeWarning(s"$where\n$msg"))
  }

  // Methods //
  def typecheck(): TypecheckResult = {
    try {
      prog.modules.foreach(typecheck(_))

      if (errors.isEmpty)
        SuccessTypecheck(warnings.toSeq)
      else
        FailTypecheck(errors.toSeq, warnings.toSeq)
    } catch {
      case e: FatalError =>
        FailTypecheck(errors.addOne(TypeError(e.getMessage)).toSeq, warnings.toSeq)
    }
  }

  private def typecheck(implicit module: Module): Unit = {
    val functions = function_env(module.name) ++ function_env
      .filter(im => module.imports.contains(im._1))
      .flatMap(im => im._2)

    module.funs.foreach(typecheck(_, functions, module))
  }

  // @todo Test
  private def typecheck(
      fun: PatternFunction,
      functions: Map[Name, PatternFunction],
      module: Module
  ): Unit = {

    val module_function_map = fun.params.map(v => v.name -> v.typ).toMap

    val res = fun.bodies.map {
      typecheck(_)(new TypeContext(fun.name, functions, module, module_function_map))
    }
    val out = fun.outParams.map(ap => ap.typ)
    implicit val ctx: TypeContext = new TypeContext(fun.name, null, module, null)
    if (res.contains(TUnit)) {
      // accept explicit Unit result type annotation
      if (out.nonEmpty && (out.size > 1 || out.head != TUnit))
        addError(TypeError.expected(TTuple(out), TTuple(res)))
    } else {
      val resType = meet(res).getOrElse {
        addError(TypeError.incompatibleReturnTypes("PatternFunction"))
        TUnit
      }
      // match return type with given annotation
      resType match {
        case TTuple(ts) =>
          if (ts.length != out.length || ts.zip(out).exists(r => !subtype(r._1, r._2))) {
            addError(TypeError.expected(TTuple(out), resType))
          }
        case t =>
          if ((out.length != 1 || !subtype(t, out.head)) && out.nonEmpty) {
            addError(TypeError.expected(TTuple(out), resType))
          }
      }
    }

  }

  def typecheck(body: Body)(implicit context: TypeContext): TypeAnno = {

    val return_types: ArrayBuffer[TypeAnno] = ArrayBuffer()
    var my_context = context.tenv

    for (i <- body.stmts.indices) {
      val stm = body.stmts(i)
      val (t, m_ctx) =
        typecheck(stm, i == body.stmts.length - 1)(new TypeContext(context, my_context))
      my_context = m_ctx
      if (t.isDefined)
        return_types.addOne(t.get)
    }

    // check if all return values are compatible
    val resType =
      if(return_types.isEmpty) {
        TUnit
      }
      else {
        meet(return_types).fold[TypeAnno] {
          addError(TypeError.incompatibleReturnTypes())
          TUnit
        }(t => t)
      }
    resType
  }

  def typecheck(stm: Statement, last_in_body: Boolean)(implicit
      context: TypeContext
  ): (Option[TypeAnno], TypeEnvironment) = {
    stm match {
      case Assert(cond) =>
        val (t, te) = typecheck(cond)
        if (!subtype(t, TBool))
          addError(TypeError.expected(TBool, t, "Assert"))
        (None, te) 
      case Assign(names, exp) =>

        if (names.length == 1) { // simple assign
          (None, context.tenv + (names.head -> typecheck(exp)._1))
        } else { // tuple unpack
          typecheck(exp)._1 match {
            case tuple@TTuple(ts) =>
              if (names.length != ts.length) { // sizes need to match
                addError(TypeError.tupleUnpack(tuple, names.length))
                (None, context.tenv)
              }
              (None, context.tenv ++ names.zip(ts).map(p => p._1 -> p._2).toMap)
            case t =>
              addError(TypeError.expected(TTuple(names.map(TypeHelper.decode)), t, "Assign"))
              (None, context.tenv)
          }
        }
      case Values(name, typ) =>
        (None, context.tenv + (name -> typ))
      case e: TerminatorStatement =>
        // A terminator statement should be the last statement in a block
        if (!last_in_body)
          warnings.addOne(
            TypeWarning(s"Terminator statement is not last statement in body ($where).")
          )
        e match {
          case Yield(exp) =>
            val (t, te) = typecheck(exp)
            (Some(t), context.tenv ++ te)
          case Fail => (None, context.tenv)
        }
      case _: Statement =>
        for (e <- extensions) {
          val (outType, env, consumed) = e.typecheck(stm, last_in_body)
          if (consumed)
            return (outType, env)
        }
        throw new FatalError(TypeError(s"Unexpected statement ${stm.prettyprint("")} found ($where)."))
    }
  }

  def typecheck(
      exp: Exp
  )(implicit context: TypeContext): (TypeAnno, CoreTypechecker.TypeEnvironment) = {
    exp match {
      case Aggregate(_, _, _, _) =>
        throw new NotImplementedError("Aggregate is not supported in typechecker.")
      case Call(name, args, _) =>
        context.functions.get(name) match {
          case Some(fun) =>
            val ret = fun.outParams.map(_.typ)
            if(fun.params.size != args.size) {
              addError(TypeError.sizeMismatch(fun.params.size, args.size, s"Call $name"))
            }
            val paramTypes = fun.params.map(_.typ)
            val (argTypes, envs) = args.map(typecheck).unzip
            val newEnv = envs.fold(context.tenv)(union)
            paramTypes.zip(argTypes).foreach {
              case (pTyp, aTyp) =>
                if(!subtype(aTyp, pTyp)) {
                  addError(TypeError.expected(pTyp, aTyp))
                }
            }
            if (ret.isEmpty) {
              exp.typed(TUnit)
              (TUnit, newEnv)
            } else if (ret.length == 1) {
              exp.typed(ret.head)
              (ret.head, newEnv)
            } else {
              exp.typed(TTuple(ret))
              (TTuple(ret), newEnv)
            }
          case None =>
            addError(TypeError.undefined("Function", name, s"Call $name"))
            (TUnit, context.tenv)
        }
      case Constant(lit) =>
        val t = typecheck(lit)
        exp.typed(t)
        (t, context.tenv)
      case Count(call) =>
        val (_, te) = typecheck(call)
        exp.typed(TBool)
        (TBool, te)
      case Def(exp) =>
        exp match {
          case Call(_, _, _) =>
          case PathAccess(_, _)   =>
          case _ =>
            addError(TypeError.expectedExp(Seq("Call", "PathAccess"), exp, s"Def ${exp.getClass.getName}"))
        }
        val (_, te) = typecheck(exp)
        exp.typed(TBool)
        (TBool, te)
      case Undef(exp) =>
        exp match {
          case Call(_, _, _) =>
          case PathAccess(_, _)   =>
          case _ =>
            addError(TypeError.expectedExp(Seq("Call", "PathAccess"), exp, s"Undef ${exp.getClass.getName}"))
        }
        val (_, te) = typecheck(exp)
        exp.typed(TBool)
        (TBool, te)
      case eq@Eq(lhs, rhs) =>
        checkEq(lhs, rhs, eq)
      case neq@Neq(lhs, rhs) =>
        checkEq(lhs, rhs, neq)
      case InstanceOf(exp, ty) =>
        val (t, te) = typecheck(exp)
        exp match {
          case Var(name) =>
            if (subtype(ty, t)) {
              exp.typed(TBool)
              return (TBool, union(context.tenv.updated(name, ty), te))
            }
            else if (!subtype(t, ty))
              addError(TypeError.unrelated(ty, t, s"${exp.getClass.getName} InstanceOf ${ty.prettyprint}"))
            exp.typed(TBool)
            (TBool, context.tenv)
          case _ =>
            if (t != ty)
              throw new FatalError(TypeError(s"InstanceOf type does not match ($where)"))
            exp.typed(TBool)
            (TBool, te)
        }
      case NotInstanceOf(exp, ty) =>
        val (t, te) = typecheck(exp)
        exp match {
          case Var(name) =>
            if (subtype(ty, t)) {
              exp.typed(TBool)
              return (TBool, union(context.tenv.updated(name, ty), te))
            }
            else if (!subtype(t, ty))
              addError(TypeError.unrelated(ty, t, s"${exp.getClass.getName} NotInstanceOf ${ty.prettyprint}"))
            exp.typed(TBool)
            (TBool, context.tenv)
          case _ =>
            if (t != ty)
              throw new FatalError(TypeError(s"NotInstanceOf type does not match ($where)"))
            exp.typed(TBool)
            (TBool, te)
        }
      case Tuple(exps) =>
        val (rt, re) = exps.map(typecheck(_)).unzip
        exp.typed(TTuple(rt))
        (
          TTuple(rt),
          re.fold(context.tenv) { case (a, b) => union(a, b) }
        )
      case Var(name) =>
        if (!context.tenv.contains(name)) {
          addError(TypeError.undefined("Variable", name))
          exp.typed(TUnit)
        } else {
          exp.typed(context.tenv(name))
        }
        (context.tenv(name), context.tenv)
      case PathAccess(receiver, link) =>
        val (typ, te) = typecheck(receiver)
        val linkType = lmi.links((typ.prettyprint, link.prettyprint))
        (convertType(linkType), union(context.tenv, te))
      case eval@Eval(_, _)    =>
        try {
          val resType = EvalHelper.typecheck(eval)
          eval.typed(resType)
          (resType, context.tenv)
        } catch {
          case ScalaTypeError(msg) =>
            errors.addOne(TypeError(msg))
            (TAny, context.tenv)
        }

      case _: Exp =>
        for (e <- extensions) {
          val (ot, et, is) = e.typecheck(exp)
          if (is) {
            exp.typed(ot)
            return (ot, et)
          }
        }
        throw new FatalError(TypeError(s"Unexpected expression ${exp.prettyprint("")} found ($where)."))
    }
  }

  def typecheck(lit: Literal)(implicit context: TypeContext): TypeAnno = {
    lit match {
      case UnitLiteral       => TUnit
      case BooleanLiteral(_) => TBool
      case IntLiteral(_)     => TInt
      case LongLiteral(_)    => TLong
      case DoubleLiteral(_)  => TDouble
      case StringLiteral(_)  => TString
    }
  }

  def where(implicit context: TypeContext): String = {
    context.where
  }

  def union(env1: TypeEnvironment, env2: TypeEnvironment)(implicit
      context: TypeContext
  ): TypeEnvironment = {
    var res = env1
    for (t <- env2.keys) {
      if (res.contains(t)) {
        val m = meet(env2(t), env1(t))
        m match {
          case Some(value) => res += (t -> value)
          case None =>
            addError(TypeError.unrelated(env2(t), env1(t)))
        }
      } else
        res += (t -> env2(t))
    }
    res
  }

  private def checkEq(lhs: Exp, rhs: Exp, exp: Exp)(implicit ctx: TypeContext) = {
    val (r, l) = (typecheck(lhs), typecheck(rhs))
    if (!(subtype(r._1, l._1) && subtype(l._1, r._1))) {
      addError(TypeError.expected(l._1, r._1, "Equality"))
    }
    exp.typed(TBool)
    (TBool, union(r._2, l._2))
  }

  private def convertType(typ: Type): TypeAnno = typ match {
    case AnyType => TAny
    case ListType(ty) =>
      val inner = convertType(ty)
      inner match {
        case linked: TLinked => TList(linked)
        case anno => TList(TNode(anno.prettyprint))
      }
    case SortType(name) =>
      fastparse.parse(name, CoreParser().typeAnno(_)).get.value
  }

  def subtype(tc: TypeAnno, tp: TypeAnno): Boolean =
    (tc, tp) match {
      case (_, _) if tc == tp       => true
      case (_, TAny)                => true
      case (_: TLinked, TAnyLinked) => true
      case (TNode(name1), TNode(name2)) =>
        lmi.nodeSupertypes.containsEntry(SortType(name1) -> SortType(name2))
      case (TList(s1), TList(s2)) =>
        subtype(s1, s2)
      case (TEnumeration(s1), TEnumeration(s2)) =>
        subtype(s1, s2)
      case (TTuple(s1), TTuple(s2)) =>
        if (s1.length == s2.length)
          return s1.zip(s2).forall { case (t1_, t2_) => subtype(t1_, t2_) }
        false
      case _ => false
    }

  def meet(t1: TypeAnno, t2: TypeAnno): Option[TypeAnno] =
    if (t1 == t2) Some(t1)
    else if (subtype(t1, t2)) Some(t2)
    else if (subtype(t2, t1)) Some(t1)
    else (t1, t2) match {
      case (_: TLinked, _: TLinked) => Some(TAnyLinked)
      case (_: TLinked, _) | (_, _: TLinked) => None
      case (TUnit, _ ) | (_, TUnit) => None
      case (_, _) => Some(TAny)
    }

  def meet(types: Iterable[TypeAnno], ifEmpty: TypeAnno = TUnit): Option[TypeAnno] =
    if(types.isEmpty) {
      Some(ifEmpty)
    }
    else {
      val (first, rest) = (types.head, types.tail)
      rest.foldLeft[Option[TypeAnno]](Some(first)) {
        case (res, t) => res.fold[Option[TypeAnno]](None)(meet(_, t))
      }
    }
}
