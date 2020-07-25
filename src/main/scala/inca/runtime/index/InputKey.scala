package inca.runtime.index

import inca.runtime.index.MetaElements.{Link, LinkedType, PrimitiveType}
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey

trait InputKey[T] extends IInputKey {
  val id: T
  val arity: Int

  override def getStringID: String = id.toString
  override def getPrettyPrintableName: String = id.toString
  override def getArity: Int = arity
  override def isEnumerable: Boolean = true
}

case class NodeTypeKey(id: LinkedType) extends InputKey[LinkedType] {
  override val arity: Int = 1
}

case class PrimitiveTypeKey(id: PrimitiveType) extends InputKey[PrimitiveType] {
  override val arity: Int = 1
}

case class LinkKey(id: Link) extends InputKey[Link] {
  override val arity: Int = 2
}

trait DynamicKey extends InputKey[Any]