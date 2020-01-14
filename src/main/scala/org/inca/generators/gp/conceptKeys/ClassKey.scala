package org.inca.generators.gp.conceptKeys

import org.eclipse.viatra.query.runtime.matchers.context.IInputKey

class ClassKey(val keyId: String) extends IInputKey {
  override def getPrettyPrintableName: String = keyId

  override def getStringID: String = keyId

  override def getArity: Int = 1

  override def isEnumerable: Boolean = true
}
