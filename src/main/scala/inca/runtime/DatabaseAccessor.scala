package inca.runtime

import inca.runtime.index.MetaElements.PrimitiveValue
import inca.runtime.index.binary.{BidirectionalManyToOneIndex, BidirectionalOneToOneIndex}
import inca.runtime.index.unary.{UnaryBagIndex, UnarySetIndex}
import truechange.{LitType, Type, URI}

class DatabaseAccessor(feed: Database) {

  def nodeInstances: Map[Type, UnarySetIndex[URI]] = feed.nodeInstances.toMap
  def nodeInstancesByValue(v: URI): Map[Type, UnarySetIndex[URI]] = {
    nodeInstances.filter {
      case (_, value) => value.index(v) > 0
    }
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
}
