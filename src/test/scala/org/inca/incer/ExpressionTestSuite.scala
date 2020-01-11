package org.inca.incer

import java.util

import org.inca.incer.indices.Indices
import org.scalatest.funsuite.AnyFunSuite

class ExpressionTestSuite extends AnyFunSuite {

  test("Type hierarchy check") {
    val exp = Add(Mul(Num(1), Num(2)), Num(3))
    val indices = new Indices()
    exp.insert(indices)

    // superTypes
    assert(Indices.superTypeMap.get(classOf[Num]).contains(classOf[Exp]))
    assert(Indices.superTypeMap.get(classOf[Add]).contains(classOf[Exp]))
    assert(Indices.superTypeMap.get(classOf[Mul]).contains(classOf[Exp]))
    assert(isEmptyOrNull(Indices.superTypeMap.get(classOf[Exp])))

    // subTypes
    assert(isEmptyOrNull(Indices.subTypeMap.get(classOf[Num])))
    assert(isEmptyOrNull(Indices.subTypeMap.get(classOf[Add])))
    assert(isEmptyOrNull(Indices.subTypeMap.get(classOf[Mul])))
    assert(Indices.subTypeMap.get(classOf[Exp]).containsAll(util.Arrays.asList(classOf[Num], classOf[Add], classOf[Mul])))
  }

  def isEmptyOrNull(coll: util.Collection[_]): Boolean = coll == null || coll.isEmpty

}