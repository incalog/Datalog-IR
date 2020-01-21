package org.inca.generators.gp.model.conceptKeys

import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
import org.inca.lang.meta.NodeLink

class LinkKey(var link: NodeLink) extends IInputKey {
  override def getPrettyPrintableName: String = link.toString

  override def getStringID: String = link.toString

  override def getArity: Int = 0 // todo what is arity of this?

  override def isEnumerable: Boolean = true
}
