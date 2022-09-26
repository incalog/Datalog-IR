package inca.runtime.data

import scala.collection.mutable

case class ObjectID(typ: String, allocId: Int) extends truechange.URI {
  //override def toString: String = s"${super.toString}($typ, $allocId)"

  // -----------------------------------------------------------------------------------
  // testAttributeManagedByScala
  val attributes: mutable.Map[String, mutable.Map[Int, Any]] = mutable.Map()

  // ts is important to guarantee, that the input relations behave correctly when the copy the scala code
  def setAttr(attr: String, value: Any, ts: Int): Int = {
    println("Set:", this, attr, ts, value)
    var tsToValue = attributes.get(attr)
    if (tsToValue.isEmpty) {
      tsToValue = Some(mutable.Map[Int, Any]())
      attributes.put(attr, tsToValue.get)
    }
    val hasValue = tsToValue.get.keys.exists(_ == ts)
    if (!hasValue) {
      tsToValue.get.put(ts, value)
    } else if (tsToValue.get(ts) != value) {
      throw new RuntimeException("Can not assign value for already defined timestamp.")
    }
    // Just return the ts to enforce a specific order inside datalog
    println("Set done:", this, attr, value, ts)
    ts + 1
  }

  def getAttr(attr: String, ts: Int): Any = {
    println("Get:", this, attr, ts, attributes)
    val tsToValue = attributes.get(attr)
    // find the next nearest, smaller timestamp inside the map
    val nullObj = ObjectID("Null", -1) // TODO: null is a bad idea. For scala types we need scala null type, but therefore we need type information or at least an isScala flag
    if (tsToValue.isEmpty) {
      println("Get done:", this, nullObj)
      nullObj
    } else {
      val res = tsToValue.get.toList
        .filter(_._1 <= ts)
        .sortBy(_._1)
        .map(_._2)
        .lastOption
        .getOrElse(nullObj)
      println("Get done:", this, res)
      res
    }
  }

  // -----------------------------------------------------------------------------------
  // testCounterManagedByScala
  val timestamps: mutable.Map[String, mutable.Map[Int, Int]] = mutable.Map()

  def getTs(attr: String, tsCount: Int): Int = {
    println("Get ts...")
    var tsForAttr = timestamps.get(attr)
    if (tsForAttr.isEmpty) {
      tsForAttr = Some(mutable.Map[Int, Int]())
      timestamps.put(attr, tsForAttr.get)
    }
    val ts = tsForAttr.get.get(tsCount)
    if (ts.isEmpty) {
      val values = tsForAttr.get.valuesIterator
      val max = if (values.isEmpty) -1 else values.max
      tsForAttr.get.put(tsCount, max + 1)
      println("Get ts (inc): ", attr, tsCount, max + 1)
      max + 1
    } else {
      println("Get ts: ", attr, tsCount, ts.get)
      ts.get
    }
  }
}

object ObjectID {

  def apply(typ: String): ObjectID = {
    new ObjectID(typ, 0)
  }

  def apply(typ: String, allocId: Int): ObjectID = {
    new ObjectID(typ, allocId)
  }
}
