package inca.frontend.constraint.util

import inca.frontend.constraint.core._
import inca.frontend.constraint.typechecker.TypeHelper
import org.scalatest.funsuite.AnyFunSuite

class TypeHelperTest extends AnyFunSuite {

  test("test decode primitive types") {

    checkEq("Int", TScalaInt)
    checkEq("Double", TScalaDouble)
    checkEq("Long", TScalaLong)
    checkEq("Boolean", TScalaBoolean)
    checkEq("String", TScalaString)
    checkEq("Any", TScala("Any"))
  }

  test("test decode TTuple") {
    checkEq("(Int, Double)", TTuple(Seq(TScalaInt, TScalaDouble)))
    checkEq(
      "(String, Boolean, (Int, Double))",
      TTuple(Seq(TScalaString, TScalaBoolean, TTuple(Seq(TScalaInt, TScalaDouble))))
    )
  }

  test("test decode TNodes") {
    checkEq("Hello", TScala("Hello"))
    checkEq("inca.analyzedLangs.Nat.Nat", TScala("inca.analyzedLangs.Nat.Nat"))
  }

  test("test decode TList") {
    checkEq("List[Hello]", TScala("List[Hello]"))
    checkEq("List[Int]", TScala("List[Int]"))
    checkEq("List[String]", TScala("List[String]"))
  }

  test("test decode strip traits") {
    checkEq("Hello with Product", TScala("Hello with Product"))
    checkEq(
      "inca.Nat with Product with SuperProduct",
      TScala("inca.Nat with Product with SuperProduct")
    )
  }

  private def checkEq(name: String, exp: Type): Unit = {
    val typ = TypeHelper.decode(name)
    assert(typ.isRight)
    assert(typ.getOrElse(throw new AssertionError("Decode threw an error")) == exp)
  }
}
