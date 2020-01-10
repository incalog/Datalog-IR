package org.inca.incer

import org.inca.incer.indices.{CollectionsFactory, Indices}
import org.scalatest.funsuite.AnyFunSuite

class ExpressionTestSuite extends AnyFunSuite {

//  test("example") {
//    val exp = Add(Add(Num(1), Num(2)), Num(3))
//    val indices = new Indices(CollectionsFactory.JAVA)
//
//    ExpTypes.insert(indices)
//    exp.insert(indices)
//    println(indices)
//  }

  test("example") {

    val f = Foo()
    val f2 = Foo()
    val i = Test(10, Some(f), f2, List(f))
    println(i)
    println(Indices.registered)
  }

}


object Bar {
  println("loaded Bar")
}