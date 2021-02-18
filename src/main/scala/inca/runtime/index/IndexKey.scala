package inca.runtime.index

import inca.runtime.index.MetaElements.Link
import inca.runtime.index.virtual.VirtualIndexFactory
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
import truechange.{LitType, Type}

sealed trait IndexKey[T] extends IInputKey {
  override def toString: String = getPrettyPrintableName
  override def getPrettyPrintableName: String = getStringID
  /** See isStateless in [[org.eclipse.viatra.query.runtime.matchers.context.IQueryMetaContext]]. */
  def isStateless: Boolean = false
}

case class NamedRelationKey(name: String, arity: Int) extends IndexKey[String] {
  override def getStringID: String = s"namedRelation#$name($arity)"
  override def getArity: Int = arity
  override def isEnumerable: Boolean = true
}

case class NodeTypeKey(id: Type) extends IndexKey[Type] {
  override val getStringID: String = "nodeType#" + id.toString
  override val getArity: Int = 1
  override def isEnumerable: Boolean = true
}

case class PrimitiveTypeKey(id: LitType) extends IndexKey[LitType] {
  override val getStringID: String = "primitiveType#" + id.toString
  override val getArity: Int = 1
  override def isEnumerable: Boolean = true
}

case class LinkNodeKey(id: Link) extends IndexKey[Link] {
  override val getStringID: String = "linkNode#" + id.toString
  override val getArity: Int = 2
  override def isEnumerable: Boolean = true
}

case class LinkPrimitiveKey(id: Link) extends IndexKey[Link] {
  override val getStringID: String = "linkPrimitive#" + id.toString
  override val getArity: Int = 2
  override def isEnumerable: Boolean = true
}

object LinkListFirstKey extends IndexKey[String] {
  override def getStringID: String = "#first"
  override def getArity: Int = 2
  override def isEnumerable: Boolean = true
}

object LinkListNextKey extends IndexKey[String] {
  override def getStringID: String = "#next"
  override def getArity: Int = 2
  override def isEnumerable: Boolean = true
}

trait DynamicKey extends IndexKey[Any]

trait VirtualKey extends IndexKey[Any] {
  def factory: VirtualIndexFactory
}