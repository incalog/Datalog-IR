package org.inca.generators.gp.model.conceptKeys

import org.eclipse.viatra.query.runtime.matchers.context.IInputKey

class ClassKey(val keyId: Class[_]) extends IInputKey {
  override def getPrettyPrintableName: String = keyId.toString

  override def getStringID: String = keyId.toString

  override def getArity: Int = 1

  override def isEnumerable: Boolean = true
}
