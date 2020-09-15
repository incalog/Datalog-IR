package inca.frontend.typechecker;

import inca.frontend.parser.Programm
import inca.frontend.core.Core._
import inca.runtime.context._
import org.scalactic.Bool
import scala.collection.mutable

sealed trait TypecheckResult
case class SuccessTypecheck(warnings: Seq[TypeWarning]) extends TypecheckResult
case class FailTypecheck(error: TypeError, warnings: Seq[TypeWarning])
    extends TypecheckResult

case class TypeWarning(msg: String)
class TypeError(msg: String) extends Exception(msg)

case class TypeContext(functions: Map[Name, PatternFunction], module: Module) {
  var variable_map: Map[String, TypeAnno] = Map() // !important mutable
}

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

  val warnings: mutable.ArrayBuffer[TypeWarning] = mutable.ArrayBuffer()

  val function_env: Map[Name, Map[Name, PatternFunction]] =
    prog.modules.map(m => m.name -> m.funs.map(f => f.name -> f).toMap).toMap

  def typecheck(): TypecheckResult = {
    try {
      ???
      SuccessTypecheck(warnings.toSeq)
    } catch {
      case e: TypeError => FailTypecheck(e, warnings.toSeq)
    }
  }

  private def typecheck(implicit module: Module) = {
    val defined_functions = function_env(module.name) ++ function_env
      .filter(im => module.imports.contains(im._1))
      .flatMap(im => im._2)

    true
  }

  private def typecheck(fun: PatternFunction)(implicit functions: Map[Name, PatternFunction], module: Module) = {
    ???

    // fun.bodies.map(typecheck(_, TypeContext), ...)
  }

  private def typecheck(body: Body)(implicit context: TypeContext) = {}

  private def typecheck(stm: Statement)(implicit context: TypeContext)  = {}

  private def typecheck(exp: Exp)(implicit context: TypeContext): TypeAnno = {
    ???
  }

}
