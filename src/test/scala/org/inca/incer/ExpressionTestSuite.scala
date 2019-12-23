package org.inca.incer

import org.inca.incer.indices.{CollectionsFactory, Indices}
import org.scalatest.funsuite.AnyFunSuite

class ExpressionTestSuite extends AnyFunSuite {

  test("example") {
    val exp = Add(Add(Num(1), Num(2)), Num(3))
    val indices = new Indices(CollectionsFactory.JAVA)

    ExpTypes.insert(indices)
    exp.insert(indices)
    println(indices)
  }

}
