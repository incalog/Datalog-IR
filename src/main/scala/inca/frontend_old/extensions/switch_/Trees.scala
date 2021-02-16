package inca.frontend_old.extensions.switch_

import inca.frontend_old.core.tree._

object Trees {
  case class Switch(bodies: Seq[Body]) extends Statement {
    override def boundVars: Set[Name] = bodies.flatMap(_.boundVars).toSet
    override def allVars: Map[Name, Option[Type]] = bodies.flatMap(_.allVars).toMap

    override def prettyprint(implicit indent: String): String = {
      if (bodies.isEmpty) "switch { }" else
        s"${indent}switch " + bodies.map(_.prettyprint(indent)).mkString(" union ")
    }
  }
}
