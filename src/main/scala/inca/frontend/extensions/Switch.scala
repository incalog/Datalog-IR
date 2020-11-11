package inca.frontend.extensions

import inca.frontend.Frontend
import inca.frontend.core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.frontend.typechecker.{NoYield, StmType}
import inca.util.Gensym

case class Switch(bodies: Seq[Body]) extends Statement {
  override def boundVars: Set[Name] = bodies.flatMap(_.boundVars).toSet
  override def allVars: Map[Name, Option[Type]] = bodies.flatMap(_.allVars).toMap

  override def prettyprint(implicit indent: String): String = {
    if (bodies.isEmpty) "switch { }" else
      s"${indent}switch " + bodies.map(_.prettyprint(indent)).mkString(" union ")
  }
}

/**
 * Extension adding "switch" statements to @see Parser.
 */
trait SwitchFrontend extends Frontend {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected def desugarables: Seq[Desugarable] = Switch +: super.desugarables

  override protected[frontend] def keywords: Set[String] = super.keywords + "switch"

  override protected[frontend] def statement[_: P]: P[Statement] =
    P("switch" ~ body.rep(sep = "union")).mapWithLoc(Switch.apply) | super.statement

  override protected def typecheckInternal(stm: Statement, mustYield: Boolean): StmType = stm match {
    case Switch(bodies) =>
      if (bodies.isEmpty) {
        error("empty switch statements are not allowed", stm)
        NoYield
      } else
        bodies.map(typecheck(_, mustYield)).reduce(stmMeet(_, _, lang))

    case _ => super.typecheckInternal(stm, mustYield)
  }
}

object Switch extends Desugarable {
  override def trans(): DesugarTrans = new DesugarTrans {
    override def desugarBody(body: Body)(implicit gensym: Gensym): Seq[Body] = {
      val isSwitch: Statement => Boolean = { case Switch(_) => true;  case _ => false }
      val (beforeSwitch, fromSwitch) = body.stmts.span(!isSwitch(_))

      if (fromSwitch.isEmpty)
        super.desugarBody(body)
      else {
        val switch = fromSwitch.head.asInstanceOf[Switch]
        val afterSwitch = fromSwitch.tail

        if (switch.bodies.isEmpty) {
          throw new IllegalArgumentException(s"Empty switch statements are not allowed $switch")
        } else {
          val alternativeBodies = switch.bodies.map(b => Body(beforeSwitch ++ b.stmts ++ afterSwitch))
          changed(alternativeBodies)
        }
      }
    }
  }
}
