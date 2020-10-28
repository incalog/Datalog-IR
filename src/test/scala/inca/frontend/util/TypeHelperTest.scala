package inca.frontend.util

import inca.frontend.core.Core._
import org.scalatest.funsuite.AnyFunSuite

class TypeHelperTest extends AnyFunSuite{

  test("test decode primitive types") {

    checkEq("Int", TInt)
    checkEq("Double", TDouble)
    checkEq("Long", TLong)
    checkEq("Boolean", TBool)
    checkEq("String", TString)
    checkEq("Any", TAny)
  }

  test("test decode refined types") {
    checkEq("Float", TDouble)
    checkEq("Short", TInt)
    checkEq("Byte", TInt)
    checkEq("Char", TInt)
  }

  test("test decode TTuple") {
    checkEq("(Int, Double)", TTuple(Seq(TInt, TDouble)))
    checkEq("(String, Boolean, (Int, Double))", TTuple(Seq(TString, TBool, TTuple(Seq(TInt, TDouble)))))
  }

  test("test decode TNodes") {
    checkEq("Hello", TNode("Hello"))
    checkEq("inca.analyzedLangs.Nat.Nat", TNode("inca.analyzedLangs.Nat.Nat"))
  }

  test("test decode TList") {
    checkEq("List[Hello]", TList(TNode("Hello")))
  }

  test("test decode TList no primitive") {
    assertResult(None) {
      TypeHelper.decode("List[Int]")
    }

    assertResult(None) {
      TypeHelper.decode("List[String]")
    }

    assertResult(None) {
      TypeHelper.decode("List[(Int, Double)]")
    }
  }

  test("test decode strip traits") {
    checkEq("Hello with Product", TNode("Hello"))
    checkEq("inca.Nat with Product with SuperProduct", TNode("inca.Nat"))
  }

  private def checkEq(name: String, exp: TypeAnno): Unit = {
    val typ = TypeHelper.decode(name)
    assert(typ.get == exp)
  }
}
