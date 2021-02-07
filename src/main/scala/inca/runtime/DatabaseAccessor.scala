package inca.runtime

import inca.runtime.index.MetaElements.PrimitiveValue
import inca.runtime.index.binary.{BidirectionalManyToOneIndex, BidirectionalOneToOneIndex}
import inca.runtime.index.unary.{UnaryBagIndex, UnarySetIndex}
import truechange.{LitType, Type, URI}

import scala.collection.mutable

class DatabaseAccessor(feed: Database) {

  def nodeInstances: mutable.Map[Type, UnarySetIndex[URI]] = feed.nodeInstances
  def nodeInstancesByValue(v: Any): mutable.Map[Type, UnarySetIndex[URI]] = {
    feed.nodeInstances.filter(_._2.entries.exists(_.equals(v)))
  }

  def primitiveInstances: mutable.Map[LitType, UnaryBagIndex[PrimitiveValue]] = feed.primitiveInstances
  def primitiveInstancesByValue(v: Any): mutable.Map[LitType, UnaryBagIndex[PrimitiveValue]] = {
    feed.primitiveInstances.filter(_._2.entries.exists(_.equals(v)))
  }

  def linkNodeInstances: mutable.Map[(String, String), BidirectionalOneToOneIndex[URI, URI]] = feed.linkNodeInstances
  def linkNodeInstancesByValue1(k: Any): mutable.Map[(String, String), BidirectionalOneToOneIndex[URI, URI]] = {
    feed.linkNodeInstances.filter(_._2.index.containsKey(k))
  }
  def linkNodeInstancesByValue2(v: Any): mutable.Map[(String, String), BidirectionalOneToOneIndex[URI, URI]] = {
    feed.linkNodeInstances.filter(_._2.index.containsValue(v))
  }

  def linkPrimitiveInstances: mutable.Map[(String, String), BidirectionalManyToOneIndex[URI, PrimitiveValue]] = feed.linkPrimitiveInstances
  def linkPrimitiveInstancesByValue1(k: Any): mutable.Map[(String, String), BidirectionalManyToOneIndex[URI, PrimitiveValue]] = {
    feed.linkPrimitiveInstances.filter(_._2.entries.exists(_._1.equals(k)))
  }

  def linkListFirstInstances: BidirectionalOneToOneIndex[URI, URI] = feed.linkListFirstInstances

  def linkListNextInstances: BidirectionalOneToOneIndex[URI, URI] = feed.linkListNextInstances
}
