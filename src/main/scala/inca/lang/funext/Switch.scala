package inca.lang.funext

import inca.lang.fun.Fun._
import inca.lang.funext.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

case class Switch(bodies: Seq[Body]) extends Statement {
  override def usedvars: Map[Name, Option[TypeAnno]] = collectUsedvars(bodies)

  override def prettyprint(implicit indent: String): String = {
    if (bodies.isEmpty) "switch { }" else
      s"${indent}switch " + bodies.map(_.prettyprint(indent)).mkString(" union ")
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
