package org.inca.generators.gp.model.keys

import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
import org.inca.meta.MetaElements.NodeType

class ClassKey(keyId: NodeType) extends IInputKey {
  override def getPrettyPrintableName: String = keyId.toString

  override def getStringID: String = keyId.toString

  override def getArity: Int = 1

  override def isEnumerable: Boolean = true
}
