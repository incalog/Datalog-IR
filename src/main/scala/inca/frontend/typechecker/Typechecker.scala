package inca.frontend.typechecker;

import inca.frontend.parser.Programm
import inca.frontend.core.Core._
import inca.runtime.context._
import org.scalactic.Bool
import scala.collection.mutable

sealed trait TypecheckResult 
case class SuccessTypecheck(warnings : Seq[TypeWarning]) extends TypecheckResult
case class FailTypecheck(error: TypeError, warnings: Seq[TypeWarning]) extends TypecheckResult


case class TypeWarning(msg: String)
class TypeError(msg: String) extends Exception(msg)

class Typechecker(lmi : LanguageMetaInfo, prog : Programm) {

  val context : Module = ???
  val type_context : Map[String, TypeAnno] = ???

  val warnings : mutable.ArrayBuffer[TypeWarning] = mutable.ArrayBuffer()

  val function_env : Map[Name, Map[Name, PatternFunction]] =
    prog.modules.map(m => m.name -> m.funs.map(f => f.name -> f).toMap).toMap

  def typecheck() : TypecheckResult = {
    try {
      ???
      SuccessTypecheck(warnings.toSeq)
    }
    catch {
      case e : TypeError => FailTypecheck(e, warnings.toSeq)
    }
  }

  private def typecheck(m : Module) = {
    val defined_functions = function_env(m.name) ++ function_env.filter(im => m.imports.contains(im._1)).flatMap(im => im._2)
    ???
  }

}