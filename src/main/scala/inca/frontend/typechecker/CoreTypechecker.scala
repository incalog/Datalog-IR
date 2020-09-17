package inca.frontend.typechecker;

import inca.frontend.parser.Program
import inca.frontend.core.Core._
import inca.frontend.typechecker.CoreTypechecker.TypeEnvironment
import inca.runtime.context._
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

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
object CoreTypechecker { type TypeEnvironment = Map[String, TypeAnno] }

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
      // check if all blocks have the same return type // @todo type hierarchy
      if (res.count(res.head == _) != res.length)
        errors.addOne(
          TypeError(
            s"Patternfunction does not have the same return type in all blocks ($w)."
          )
        )

      // match return type with given annotation
      res.head match {
        case TTuple(ts) =>
          if ( // @todo type hierarchy
            ts.length != out.length || ts.zip(out).exists(r => r._1 != r._2)
          )
            errors.addOne(
              TypeError(
                s"Annotated return type does not match ($w, Code: 0x02)"
              )
            )
        case t =>
          if ((out.length != 1 || out.head != t) && out.nonEmpty) { // @todo type hierarchy
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

  private def typecheck(body: Body)(implicit context: TypeContext): TypeAnno = {

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
    if (return_types.exists(_ != return_types.head)) // @todo type hierarchy
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
        if (t != TBool) // @todo hierarchy
          errors.addOne(
            TypeError(s"Assert condition does not evaluate to bool ($where).")
          )
        (None, context.tenv ++ te) // @todo hierarchy
      case Assign(names, exp) => // @todo check for already in use
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
        throw new FatalError(s"Unexpected statement ${stm.prettyprint("")} found ($where).")
      // @todo Extensions // @note Might be a terminator statement (or contain one); flag maybe?
    }
  }

  def typecheck(
      exp: Exp
  )(implicit context: TypeContext): (TypeAnno, CoreTypechecker.TypeEnvironment) = {
    exp match {
      case Aggregate(init, join, unjoin, call) => ???
      case Call(name, args, transitive) =>
        context.functions.get(name) match {
          case Some(fun) =>
            val ret = fun.outParams.map(_.typ)
            if (ret.isEmpty)
              null
            else if (ret.length == 1)
              (ret.head, context.tenv)
            else
              (TTuple(ret), context.tenv)
          case None =>
            throw new FatalError(s"Function $name is not defined ($where).")
        }
      case Constant(lit) => (typecheck(lit), context.tenv)
      case Count(call) =>
        val (_, te) = typecheck(call)
        (TInt, context.tenv ++ te)
      case Def(exp) =>
        val (_, te) = typecheck(exp) // @todo Restrictions ?
        (TBool, context.tenv ++ te)
      case Undef(exp) =>
        val (_, te) = typecheck(exp) // @todo Restrictions ?
        (TBool, context.tenv ++ te)
      case Eq(lhs, rhs) =>
        val r, l = (typecheck(lhs), typecheck(rhs))
        if (r != l) // @todo
          throw new FatalError(s"Equality operands do not match ($where).")
        (TBool, context.tenv)
      case Neq(lhs, rhs) =>
        val r, l = (typecheck(lhs), typecheck(rhs))
        if (r != l) // @todo
          throw new FatalError(s"Inequality operands do not match ($where).")
        (TBool, context.tenv)
      case InstanceOf(exp, ty) =>
        val (t, te) = typecheck(exp)
        exp match {
          case Var(name) =>
            // @todo type hierachy and compile time evaluation?
            (TBool, te ++ context.tenv.updated(name, ty))
          case _ =>
            if (t != ty)
              throw new FatalError(s"InstanceOf type does not match ($where, Code: 0x01)")
            (TBool, context.tenv ++ te)
        }
      case NotInstanceOf(exp, ty) =>
        val (t, te) = typecheck(exp)
        exp match {
          case Var(name) =>
            // @todo type hierachy and compile time evaluation?
            (TBool, te ++ context.tenv.updated(name, ty))
          case _ =>
            if (t != ty)
              throw new FatalError(
                s"NotInstanceOf type does not match ($where, Code: 0x01)"
              )
            (TBool, context.tenv ++ te)
        }
      case Tuple(exps) =>
        val (rt, re) = exps.map(typecheck(_)).unzip
        (
          TTuple(rt),
          re.fold(context.tenv) { case (a, b) => a ++ b }
        ) // @todo type hierachy
      case Var(name) =>
        if (!context.tenv.contains(name))
          throw new FatalError(
            s"Variable $name is not defined ${where}"
          )
        (context.tenv(name), context.tenv)
      case PathAccess(receiver, link)     => ???
      case Eval(params, code) => ???
      case _: Exp =>
        for (e <- extensions) {
          val (ot, et, is) = e.typecheck(exp)
          if (is)
            return (ot, et)
        }
        throw new FatalError(s"Unexpected expression ${exp.prettyprint("")} found ($where).")
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
}
