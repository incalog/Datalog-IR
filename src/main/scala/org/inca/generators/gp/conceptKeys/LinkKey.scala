package org.inca.generators.gp.conceptKeys

import org.eclipse.viatra.query.runtime.matchers.context.IInputKey

class LinkKey(var keyId: String) extends IInputKey {
  override def getPrettyPrintableName: String = keyId

  override def getStringID: String = keyId

  override def getArity: Int = 0 // todo what is arity of this?

  override def isEnumerable: Boolean = true
}
