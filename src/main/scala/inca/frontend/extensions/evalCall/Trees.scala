package inca.frontend.extensions.evalCall

import inca.frontend.core.tree._
import inca.frontend.parser.SourceLocation
import inca.util.Meta.Scala

object Trees {
  case class EvalCall(fun: Eval, args: Seq[Expression]) extends Expression with SourceLocation {
    override def freeVars: Map[Name, Option[Type]] = args.flatMap(_.freeVars).toMap

    override def prettyprint(implicit indent: String): String =
      s"`${fun.code.syntax}`(${args.map(_.prettyprint).mkString(", ")})"
  }
  object EvalCall {
    def apply(code: Scala[meta.Term], args: Seq[Expression]): EvalCall = new EvalCall(Eval(code), args)
  }
}
