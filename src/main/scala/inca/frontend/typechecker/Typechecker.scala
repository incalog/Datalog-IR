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

    if (res.contains(null)) {
      if (out.length != 0)
        throw new TypeError(
          s"Patternfunction ${fun.name} in ${module.name} does not return the annotated type. 0x00"
        )
    } else {
      // check if all blocks have the same return type // @todo type hierachy
      if (res.filter(res.head == _).length != res.length)
        throw new TypeError(
          s"Patternfunction ${fun.name} in ${module.name} does not return the same type in all blocks."
        )

      // match return type with given annotation
      res.head match {
        case TTuple(ts) => {
          if (
            ts.length != out.length || ts.zip(out).filter(r => r._1 != r._2).length != 0
          )
            throw new TypeError(
              s"Patternfunction ${fun.name} in ${module.name} does not return the annotated type. 0x01"
            )
        }
        case t => {
          if ((out.length != 1 || out.head != t) && out.length != 0)
            throw new TypeError(
              s"Patternfunction ${fun.name} in ${module.name} does not return the annotated type. 0x02"
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
            throw new TypeError("Assert condition does not evaluate to bool.")
        case Assign(names, exp) => {
          if (names.length == 1) // simple assign
            context.variable_map += names.head -> typecheck(exp)
          else { // tuple unpack
            typecheck(exp) match {
              case TTuple(ts) => {
                if (names.length != ts.length) // sizes need to match
                  throw new TypeError(
                    s"Cannot unpack tuple with ${ts.length} to tuple with ${names.length} members."
                  )
                context.variable_map ++= names.zip(ts).map(p => p._1 -> p._2).toMap
              }
              case t =>
                throw new TypeError(
                  s"Cannot unpack type ${t.prettyprint} to tuple with ${names.length} members."
                )
            }
          }
        }
        case Values(name, typ) => context.variable_map += name -> typ
        case e: TerminatorStatement => {
          // A terminator statement should be the last statement in a block
          if (i < body.stmts.length - 1)
            warnings.addOne(
              TypeWarning("Terminator statement is not last statement in body.")
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
        s"Body has multiple return values in function ${context.fname} in module ${context.module.name}"
      )

    if (return_types.length > 0) return_types.head
    else null
  }

  private def typecheck(exp: Exp)(implicit context: TypeContext): TypeAnno = {
    exp match {
      case Aggregate(init, join, unjoin, call) => ???
      case Call(name, args, transitive)        => ???
      case Constant(lit)                       => typecheck(lit)
      case Count(call)                         => ???
      case Def(exp)                            => ???
      case Undef(exp)                          => ???
      case Eq(lhs, rhs)                        => ???
      case Neq(lhs, rhs)                       => ???
      case InstanceOf(exp, ty)                 => ???
      case NotInstanceOf(exp, ty)              => ???
      case Tuple(exps)                         => TTuple(exps.map(typecheck))
      case Var(name) => {
        if (!context.variable_map.contains(name))
          throw new TypeError(
            s"Variable $name is not defined in Patternfunction ${context.fname} in Module ${context.module.name}"
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
}
