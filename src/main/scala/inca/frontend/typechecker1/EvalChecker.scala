package inca.frontend.typechecker1

import inca.frontend.core.Core.{Eval, TypeAnno}
import inca.frontend.util.TypeHelper

object EvalChecker {
  /**
   * Computes the result type of an Eval expression and validates the contained Scala code for type correctness
   * However it does not set the type of the eval expression. This is up to the caller
   * @param eval the eval expression
   * @param ctx the type context of the type check run
   * @return the type annotation of the result type
   */
  def typecheck(eval: Eval)(implicit ctx: TypeContext): TypeAnno = {
    import scala.reflect.runtime.currentMirror
    import scala.tools.reflect.{ToolBox, ToolBoxError}

    val params = eval.params
    // if we didn't find the free variable it might be a package so we ignore it and let the compiler figure it out
    val env = params.filter(ctx.tenv.contains).map(p => p -> ctx.tenv(p))
    // here we use a little hack. We create one big block that defines all the params with their type
    // but because they need to be initialized as well we simply throw an exception everytime
    // because throw is an expression that results in the bottom type Nothing so the typechecker is happy
    val paramString = env.map {
      case (name, typ) => s"val $name : ${typ.prettyprint} = throw new Exception()"
    }.mkString("; ")
    val codeSource = s"{$paramString; ${eval.code.syntax}}"
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(codeSource)
    try {
      val typechecked = toolbox.typecheck(tree)
      val typ = typechecked.tpe.dealias
      TypeHelper.decode(typ.toString)
    } catch {
      // throw a different exception to hide impl details
      case ToolBoxError(msg, _) => throw ScalaTypeError(msg)
    }
  }
}

case class ScalaTypeError(msg: String) extends Exception(msg)
