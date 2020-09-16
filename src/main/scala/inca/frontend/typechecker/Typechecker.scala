package inca.frontend.typechecker;

import inca.frontend.parser.Programm
import inca.frontend.core.Core._
import inca.runtime.context._
import org.scalactic.Bool
import scala.collection.mutable.ArrayBuffer

sealed trait TypecheckResult
case class SuccessTypecheck(warnings: Seq[TypeWarning]) extends TypecheckResult
case class FailTypecheck(error: TypeError, warnings: Seq[TypeWarning])
    extends TypecheckResult

case class TypeWarning(msg: String)
class TypeError(msg: String) extends Exception(msg)

case class TypeContext(
    fname: String, // Name of the function (For error messages)
    functions: Map[Name, PatternFunction], // Functions accessable from the module
    module: Module, // The module the function is in
    var variable_map: Map[String, TypeAnno] = Map() // The variable type context
)

/** IncA Typechecker
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  *
  * @version 0.0
  *
  * @param   lmi
  * @param   prog
  */
class Typechecker(lmi: LanguageMetaInfo, prog: Programm) {

  val warnings: ArrayBuffer[TypeWarning] = ArrayBuffer()

  val function_env: Map[Name, Map[Name, PatternFunction]] =
    prog.modules.map(m => m.name -> m.funs.map(f => f.name -> f).toMap).toMap

  def typecheck(): TypecheckResult = {
    try {
      prog.modules.map(typecheck(_))
      SuccessTypecheck(warnings.toSeq)
    } catch {
      case e: TypeError => FailTypecheck(e, warnings.toSeq)
    }
  }

  private def typecheck(implicit module: Module): Unit = {
    val functions = function_env(module.name) ++ function_env
      .filter(im => module.imports.contains(im._1))
      .flatMap(im => im._2)

    module.funs.map(typecheck(_, functions, module))
  }

  // @todo Test
  private def typecheck(
      fun: PatternFunction,
      functions: Map[Name, PatternFunction],
      module: Module
  ): Unit = {

    val init_map = fun.params.map(v => v.name -> v.typ).toMap

    val res = fun.bodies.map {
      typecheck(_)(TypeContext(fun.name, functions, module, init_map))
    }
    val out = fun.outParams.map(ap => ap.typ)
    val w = where(TypeContext(fun.name, null, module, null))

    if (res.contains(null)) {
      if (out.length != 0)
        throw new TypeError(
          s"Annotated return type does not match ($w, Code: 0x01)"
        )
    } else {
      // check if all blocks have the same return type // @todo type hierachy
      if (res.filter(res.head == _).length != res.length)
        throw new TypeError(
          s"Patternfunction does not have the same return type in all blocks ($w)."
        )

      // match return type with given annotation
      res.head match {
        case TTuple(ts) => {
          if (
            ts.length != out.length || ts.zip(out).filter(r => r._1 != r._2).length != 0
          )
            throw new TypeError(
              s"Annotated return type does not match ($w, Code: 0x02)"
            )
        }
        case t => {
          if ((out.length != 1 || out.head != t) && out.length != 0)
            throw new TypeError(
              s"Annotated return type does not match ($w, Code: 0x03)"
            )
        }
      }
    }

  }

  private def typecheck(body: Body)(implicit context: TypeContext): TypeAnno = {

    var return_types: ArrayBuffer[TypeAnno] = ArrayBuffer()

    for (i <- 0 until body.stmts.length) {
      val stm = body.stmts(i)
      stm match {
        case Assert(cond) =>
          if (typecheck(cond) != TBool)
            throw new TypeError(s"Assert condition does not evaluate to bool (${where}).")
        case Assign(names, exp) => {
          if (names.length == 1) // simple assign
            context.variable_map += names.head -> typecheck(exp)
          else { // tuple unpack
            typecheck(exp) match {
              case TTuple(ts) => {
                if (names.length != ts.length) // sizes need to match
                  throw new TypeError(
                    s"Cannot unpack tuple with ${ts.length} to tuple with ${names.length} members (${where})."
                  )
                context.variable_map ++= names.zip(ts).map(p => p._1 -> p._2).toMap
              }
              case t =>
                throw new TypeError(
                  s"Cannot unpack type ${t.prettyprint} to tuple with ${names.length} members (${where})."
                )
            }
          }
        }
        case Values(name, typ) => context.variable_map += name -> typ
        case e: TerminatorStatement => {
          // A terminator statement should be the last statement in a block
          if (i < body.stmts.length - 1)
            warnings.addOne(
              TypeWarning(s"Terminator statement is not last statement in body (${where}).")
            )
          e match {
            case Yield(exp) => return_types += typecheck(exp)
            case Fail       => return_types += null
          }
        }
        case _: Statement =>
          ??? // @todo Extensions // @note Might be a terminator statement (or contain one); flag maybe?
      }
    }

    // check if all return values are the same // @todo type hierachy
    if (return_types.filter(_ != return_types.head).length != 0)
      throw new TypeError(
        s"Body has multiple return values (${where})."
      )

    if (return_types.length > 0) return_types.head
    else null
  }

  private def typecheck(exp: Exp)(implicit context: TypeContext): TypeAnno = {
    exp match {
      case Aggregate(init, join, unjoin, call) => ???
      case Call(name, args, transitive)        => {
        if (context.functions.contains(name)) {
          val fun = context.functions(name)
          val ret = fun.outParams.map(_.typ)
          if (ret.isEmpty)
            null 
          else if (ret.length == 1)
            ret.head
          else 
            TTuple(ret)
        }
        else 
          throw new TypeError(s"Function $name is not defined (${where}).")
      }
      case Constant(lit)                       => typecheck(lit)
      case Count(call)                         => {
        typecheck(call)
        TInt
      }
      case Def(exp)                            => {
        typecheck(exp) // @todo Restrictions ?
        TBool
      }
      case Undef(exp)                          => {
        typecheck(exp) // @todo Restrictions ?
        TBool
      }
      case Eq(lhs, rhs)                        => {
        val r, l = (typecheck(lhs), typecheck(rhs))
        if (r != l)
          throw new TypeError(s"Equality operands do not match $where.")
        TBool
      }
      case Neq(lhs, rhs)                       => {
        val r, l = (typecheck(lhs), typecheck(rhs))
        if (r != l)
          throw new TypeError(s"Inequality operands do not match $where.")
        TBool
      }
      case InstanceOf(exp, ty)                 => {
        val ety = typecheck(exp)
        exp match {
          case Var(name) => {
            // @todo type hierachy and compile time evaluation?
            context.variable_map = context.variable_map.updated(name, ety)
            ty
          }
          case _ => {
            if (ety != ty) 
              throw new TypeError(s"InstanceOf type does not match ($where, Code: 0x01)")
            ty
          }
        }
      }
      case NotInstanceOf(exp, ty)              => {
        val ety = typecheck(exp)
        exp match {
          case Var(name) => {
            // @todo type hierachy and compile time evaluation?
            context.variable_map = context.variable_map.updated(name, ety)
            ty
          }
          case _ => {
            if (ety != ty) 
              throw new TypeError(s"NotInstanceOf type does not match ($where, Code: 0x01)")
            ty
          }
        }
      }
      case Tuple(exps)                         => TTuple(exps.map(typecheck))
      case Var(name) => {
        if (!context.variable_map.contains(name))
          throw new TypeError(
            s"Variable $name is not defined ${where}"
          )
        context.variable_map(name)
      }
      case PathAccess(receiver, link)     => ???
      case Eval(params, resultType, code) => ???
      case e: Exp                         => ??? // @todo extensions
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

  private def where(implicit context : TypeContext) : String = {
    s"Function: ${context.fname}, Module: ${context.module.name}"
  }
}
