package inca.frontend.functionalxsouffle.core

import inca.frontend.functional.core
import inca.frontend.functional.core.{Call, Expression, Type, Var}

case class SouffleRelationCall(name: core.Name, args: Seq[Expression]) extends Expression {
  override def vars: Map[core.Name, Option[Type]] = args.flatMap(_.vars).toMap

  override def freevars: Seq[Var] = args.flatMap(_.freevars)

  override def calls: Set[Call] = args.flatMap(_.calls).toSet

  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = s"$name(${args.map(_.prettyprint).mkString(", ")})"
}
