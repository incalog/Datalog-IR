package inca.frontend.extensions.switch_

import inca.frontend.core.tree._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.frontend.extensions.switch_.Trees._
import inca.util.Gensym

object Desugaring extends Desugarable {

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