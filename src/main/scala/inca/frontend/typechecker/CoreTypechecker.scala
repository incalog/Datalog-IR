package inca.frontend.typechecker;

import inca.frontend.core.Core._
import inca.frontend.parser.{CoreParser, Program}
import inca.frontend.typechecker.CoreTypechecker.TypeEnvironment
import inca.frontend.util.{EvalHelper, ScalaTypeError}
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
class FatalError(msg: String) extends Exception(msg)

/** TypeContext */
class TypeContext(
    val fname: String, // Name of the function (For error messages)
    val functions: Map[Name, PatternFunction], // Functions accessable from the module
    val module: Module, // The module the function is in
    val tenv: CoreTypechecker.TypeEnvironment = Map() // The variable type context
) {
  def this(tc: TypeContext, tenv: TypeEnvironment) {
    this(tc.fname, tc.functions, tc.module, tenv)
  }
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
    lmi: LanguageMetaInfo,
    prog: Program,
    extensions: Seq[TypecheckerExtension]
) {

  // Data //
  val warnings: ArrayBuffer[TypeWarning] = ArrayBuffer()
  val errors: ArrayBuffer[TypeError] = ArrayBuffer()

  val function_env: Map[Name, Map[Name, PatternFunction]] =
    prog.modules.map(m => m.name -> m.funs.map(f => f.name -> f).toMap).toMap

  extensions.foreach(_.typechecker = this)

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
    val w = where(new TypeContext(fun.name, null, module, null))

    if (res.contains(TUnit)) {
      if (out.nonEmpty)
        errors.addOne(TypeError(s"Annotated return type does not match ($w, Code: 0x01)"))
    } else {
      // check if all blocks have the same return type 
      if (res.count(x => subtype(res.head, x) && subtype(x, res.head)) != res.length)
        errors.addOne(
          TypeError(
            s"Patternfunction does not have the same return type in all blocks ($w)."
          )
        )

      // match return type with given annotation
      res.head match {
        case TTuple(ts) =>
          if (ts.length != out.length || ts.zip(out).exists(r => !subtype(r._1, r._2)))
            errors.addOne(
              TypeError(
                s"Annotated return type does not match ($w, Code: 0x02)"
              )
            )
        case t =>
          if ((out.length != 1 || !subtype(t, out.head)) && out.nonEmpty) { 
            println(t, out)
            errors.addOne(
              TypeError(
                s"Annotated return type does not match ($w, Code: 0x03)"
              )
            )
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

    // check if all return values are the same
    if (return_types.exists(x => !subtype(return_types.head, x)))
      errors.addOne(
        TypeError(
          s"Body has multiple return values (${where})."
        )
      )

    if (return_types.nonEmpty) return_types.head
    else TUnit
  }

  def typecheck(stm: Statement, last_in_body: Boolean)(implicit
      context: TypeContext
  ): (Option[TypeAnno], TypeEnvironment) = {
    stm match {
      case Assert(cond) =>
        val (t, te) = typecheck(cond)
        if (!subtype(t, TBool))
          errors.addOne(
            TypeError(s"Assert condition does not evaluate to bool ($where).")
          )
        (None, te) 
      case Assign(names, exp) => // @todo check for already in use
        if (names.exists(context.tenv.contains))
          throw new FatalError(s"Variable is already in use ($where).")

        if (names.length == 1) { // simple assign
          (None, context.tenv + (names.head -> typecheck(exp)._1))
        } else { // tuple unpack
          typecheck(exp)._1 match {
            case TTuple(ts) =>
              if (names.length != ts.length) { // sizes need to match
                errors.addOne(
                  TypeError(
                    s"Cannot unpack tuple with ${ts.length} to tuple with ${names.length} members ($where)."
                  )
                )
                (None, context.tenv)
              }
              (None, context.tenv ++ names.zip(ts).map(p => p._1 -> p._2).toMap)
            case t =>
              errors.addOne(
                TypeError(
                  s"Cannot unpack type ${t.prettyprint} to tuple with ${names.length} members ($where)."
                )
              )
              (None, context.tenv)
          }
        }
      case Values(name, typ) =>
        if (context.tenv.contains(name))
          throw new FatalError(s"Variable '$name' already in use ($where).")
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
          val (ot, et, is) = e.typecheck(stm, last_in_body)
          if (is)
            return (ot, et)
        }
        throw new FatalError(
          s"Unexpected statement ${stm.prettyprint("")} found ($where)."
        )
    }
  }

  def typecheck(
      exp: Exp
  )(implicit context: TypeContext): (TypeAnno, CoreTypechecker.TypeEnvironment) = {
    exp match {
      case Aggregate(init, join, unjoin, call) =>
        throw new NotImplementedError("Aggregate is not supported in typechecker.")
      case Call(name, args, transitive) =>
        context.functions.get(name) match {
          case Some(fun) =>
            val ret = fun.outParams.map(_.typ)
            if(fun.params.size != args.size) {
              errors.addOne(TypeError(s"function $name takes ${fun.params.size} arguments, but got ${args.size}"))
            }
            val paramTypes = fun.params.map(_.typ)
            val (argTypes, envs) = args.map(typecheck).unzip
            val newEnv = envs.fold(context.tenv)(union)
            paramTypes.zip(argTypes).foreach {
              case (pTyp, aTyp) =>
                if(!subtype(aTyp, pTyp)) {
                  errors.addOne(TypeError(s"expected $pTyp, but got $aTyp which is not a subtype of the first"))
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
            throw new FatalError(s"Function $name is not defined ($where).")
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
          case Call(name, args, transitive) =>
          case PathAccess(receiver, link)   =>
          case _ =>
            errors.addOne(
              TypeError(s"Def requires a Call or PathAccess Expression ($where).")
            )
        }
        val (_, te) = typecheck(exp)
        exp.typed(TBool)
        (TBool, te)
      case Undef(exp) =>
        exp match {
          case Call(name, args, transitive) =>
          case PathAccess(receiver, link)   =>
          case _ =>
            errors.addOne(
              TypeError(s"Undef requires a Call or PathAccess Expression ($where).")
            )
        }
        val (_, te) = typecheck(exp)
        exp.typed(TBool)
        (TBool, te)
      case Eq(lhs, rhs) =>
        val (r, l) = (typecheck(lhs), typecheck(rhs))
        if (subtype(r._1, l._1) && subtype(l._1, r._1))
          throw new FatalError(s"Equality operand types do not match ($where).")
        exp.typed(TBool)
        (TBool, union(r._2, l._2))
      case Neq(lhs, rhs) =>
        val (r, l) = (typecheck(lhs), typecheck(rhs))
        if (subtype(r._1, l._1) && subtype(l._1, r._1))
          throw new FatalError(s"Inequality operand types do not match ($where).")
        exp.typed(TBool)
        (TBool, union(r._2, l._2))
      case InstanceOf(exp, ty) =>
        val (t, te) = typecheck(exp)
        exp match {
          case Var(name) =>
            if (subtype(ty, t)) {
              exp.typed(TBool)
              return (TBool, union(context.tenv.updated(name, ty), te))
            }
            else if (!subtype(t, ty))
              errors.addOne(
                TypeError(
                  s"InstanceOf operands doesn't share a typing relation ($where)."
                )
              )
            exp.typed(TBool)
            (TBool, context.tenv)
          case _ =>
            if (t != ty)
              throw new FatalError(s"InstanceOf type does not match ($where, Code: 0x01)")
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
              errors.addOne(
                TypeError(
                  s"NotInstanceOf operands doesn't share a typing relation ($where)."
                )
              )
            exp.typed(TBool)
            (TBool, context.tenv)
          case _ =>
            if (t != ty)
              throw new FatalError(s"NotInstanceOf type does not match ($where, Code: 0x01)")
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
        if (!context.tenv.contains(name))
          throw new FatalError(
            s"Variable $name is not defined ${where}"
          )
        exp.typed(context.tenv(name))
        (context.tenv(name), context.tenv)
      case PathAccess(receiver, link) =>
        val (typ, te) = typecheck(receiver)
        val linkType = lmi.links((typ.prettyprint, link.prettyprint))
        (convertType(linkType), union(context.tenv, te))
      case eval@Eval(params, code)    =>
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
        throw new FatalError(
          s"Unexpected expression ${exp.prettyprint("")} found ($where)."
        )
    }
  }

  private def typecheck(lit: Literal)(implicit context: TypeContext): TypeAnno = {
    lit match {
      case UnitLiteral       => null
      case BooleanLiteral(v) => TBool
      case IntLiteral(v)     => TInt
      case LongLiteral(v)    => TLong
      case DoubleLiteral(v)  => TDouble
      case StringLiteral(v)  => TString
    }
  }

  def where(implicit context: TypeContext): String = {
    s"Function: ${context.fname}, Module: ${context.module.name}"
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
            errors.addOne(
              TypeError(
                s"${env2(t)} and ${env1(t)} do not share a type relationship ($where)."
              )
            )
        }
      } else
        res += (t -> env2(t))
    }
    res
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
    else if (subtype(t1, t2)) Some(t1)
    else if (subtype(t2, t1)) Some(t2)
    else None
}
