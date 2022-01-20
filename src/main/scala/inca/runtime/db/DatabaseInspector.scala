package inca.runtime.db

import inca.runtime.data.{MockURI, URIValue}
import inca.runtime.index.MetaElements.PrimitiveValue
import inca.runtime.index.binary.{BidirectionalManyToOneIndex, BidirectionalOneToOneIndex}
import inca.runtime.index.unary.{UnaryBagIndex, UnarySetIndex}
import truechange.{LitType, Type, URI}

case class DatabaseInspector(feed: Database) {

  def nodeInstances: Map[Type, UnarySetIndex[URI]] = feed.nodeInstances.toMap

  def nodeInstancesByValue(v: URI): Map[Type, UnarySetIndex[URI]] = {
    nodeInstances.filter {
      case (_, value) => value.index(v) > 0
    }
  }

  def nodeTypeOfURI(v: URI): Option[Type] = {
    val tys = nodeInstances.filter { case (_, set) => set.index(v) == 1 }.keys

    val ty = feed.dataModel.mostSpecificType(tys.toSeq)
    ty
  }

  def primitiveInstances: Map[LitType, UnaryBagIndex[PrimitiveValue]] = feed.primitiveInstances.toMap

  def primitiveInstancesByValue(v: PrimitiveValue): Map[LitType, UnaryBagIndex[PrimitiveValue]] = {
    primitiveInstances.filter {
      case (_, value) => value.index(v) > 0
    }
  }

  def linkNodeInstances: Map[(String, String), BidirectionalOneToOneIndex[URI, URI]] = feed.linkNodeInstances.toMap

  def linkNodeInstancesByValue1(k: URI): Map[(String, String), BidirectionalOneToOneIndex[URI, URI]] = {
    linkNodeInstances.filter {
      case (_, value) => value.index.containsKey(k)
    }
  }

  def linkNodeInstancesByValue2(v: URI): Map[(String, String), BidirectionalOneToOneIndex[URI, URI]] = {
    linkNodeInstances.filter {
      case (_, value) => value.index.containsValue(v)
    }
  }

  def linkPrimitiveInstances: Map[(String, String), BidirectionalManyToOneIndex[URI, PrimitiveValue]] = feed.linkPrimitiveInstances.toMap

  def linkPrimitiveInstancesByValue1(k: URI): Map[(String, String), BidirectionalManyToOneIndex[URI, PrimitiveValue]] = {
    linkPrimitiveInstances.filter {
      case (_, value) => value.entries.exists {
        case (uri, _) => uri.equals(k)
      }
    }
  }

  def linkListFirstInstances: BidirectionalOneToOneIndex[URI, URI] = feed.linkListFirstInstances

  def linkListNextInstances: BidirectionalOneToOneIndex[URI, URI] = feed.linkListNextInstances


  def nodeChildrenOfURI(uri: URI): Map[String, URI] = {
    feed.linkNodeInstances.flatMap { case ((_, link), index) =>
      if(index.index.containsKey(uri)) Some((link, index.index.get(uri)))
      else None
    }.toMap
  }

  def primitiveChildrenOfURI(uri: URI): Map[String, Any] = {
    feed.linkPrimitiveInstances.flatMap { case ((_, link), index) =>
      if(index.index(uri).nonEmpty) Some((link, index.index(uri).head))
      else None
    }.toMap
  }

  def childrenOfURI(uri: URI): Map[String, Any] = nodeChildrenOfURI(uri) ++ primitiveChildrenOfURI(uri)

  def uriMatchingString(str: String): Option[URI] =
    nodeInstances.flatMap { case (_, set) =>
      set.entries.find { uri =>
        uri.toString == str
      }
    }.headOption

  def prettyPrint(v: Any, depth: Int = Int.MinValue): String = v match {
    case uri: MockURI => MockURI.convertToValue(uri).deepPrettyPrint(this)
    case uri: URI => prettyPrint(uri)
    case uriv: URIValue =>
      uriMatchingString(uriv.id) match {
        case Some(uri) =>
          prettyPrint(uri)
        case None => throw new IllegalStateException(s"URI ${uriv.id} cannot be found in EDB")
      }
    case _ => v.toString
  }

  def prettyPrint(uri: URI): String = {
    val typTag = nodeTypeOfURI(uri) match {
      case Some(typ) => typ.toString
      case None => throw new IllegalArgumentException(s"$uri is not stored in the EDB")
    }
    val prettyChildren = childrenOfURI(uri).toSeq.map { case (link, v) => s"$link: ${prettyPrint(v)}"}
    s"$typTag(${prettyChildren.mkString(", ")})"
  }
}
