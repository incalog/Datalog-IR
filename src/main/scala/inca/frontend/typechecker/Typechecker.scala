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
    fname: String,
    functions: Map[Name, PatternFunction],
    module: Module,
    var variable_map: Map[String, TypeAnno] = Map()
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

    val res = fun.bodies.map { typecheck(_)(TypeContext(fun.name, functions, module, init_map)) }

    // check if all blocks have the same return type // @todo type hierachy
    if (res.filter(res.head == _).length != res.length)
      throw new TypeError(s"Patternfunction ${fun.name} in ${module.name} does not return the same type in all blocks.")

    // match return type with given annotation
    val out = fun.outParams.map(ap => ap.typ)
    res.head match {
      case TTuple(ts) => {
        if (ts.length != out.length || ts.zip(out).filter(r => r._1 != r._2).length != 0)
          throw new TypeError(s"Patternfunction ${fun.name} in ${module.name} does not return the annotated type.")
      }
      case t => {
        if (out.length != 1 || out.head == t)
          throw new TypeError(s"Patternfunction ${fun.name} in ${module.name} does not return the annotated type.")
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
                  throw new TypeError(s"Cannot unpack tuple with ${ts.length} to tuple with ${names.length} members.")
                context.variable_map ++= names.zip(ts).map(p => p._1 -> p._2).toMap
              }
              case t =>
                throw new TypeError(s"Cannot unpack type ${t.prettyprint} to tuple with ${names.length} members.")
            }
          }
        }
        case Values(name, typ) => context.variable_map += name -> typ
        case e: TerminatorStatement => { 
          // A terminator statement should be the last statement in a block
          if (i < body.stmts.length - 1)
            warnings.addOne(TypeWarning("Terminator statement is not last statement in body."))
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
      throw new TypeError(s"Body has multiple return values in function ${context.fname} in module ${context.module.name}")

    return_types.head
  }

  private def typecheck(exp: Exp)(implicit context: TypeContext): TypeAnno = {
    ???
  }

}
