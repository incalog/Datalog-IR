package org.inca.lang.core

import org.inca.lang.core.Content.IGenNameProvider

object Values {
  trait IValue extends IGenNameProvider
  trait IVariableValue extends IGenNameProvider with IValue

  abstract class AbstractLiteralValue extends IValue
  case class BoolValue(value: Boolean) extends AbstractLiteralValue
}
