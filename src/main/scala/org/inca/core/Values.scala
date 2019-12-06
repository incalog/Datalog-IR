package org.inca.core

import org.inca.core.Content.IGenNameProvider

object Values {
  trait IValue extends IGenNameProvider
  trait IVariableValue extends IGenNameProvider with IValue
}
