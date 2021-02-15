package inca.frontend_old.extensions.evalCall

import inca.frontend_old.core
import inca.frontend_old.core.tree._
import inca.frontend_old.parser.SourceLocation
import inca.util.Meta.Scala

trait Trees extends core.Trees with Syntax {
  override def EvalCall(fun: Eval, args: Seq[Expression]): Expression = Trees.EvalCall(fun, args)
}

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
