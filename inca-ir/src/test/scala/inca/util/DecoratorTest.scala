package inca.util

import org.scalatest.funsuite.AnyFunSuite

class DecoratorTest extends AnyFunSuite:

  test("Memoize") {
    var callCount = 0

    def test(identifier: String): Int =
      val oldCount = callCount
      callCount += 1
      oldCount

    lazy val mTest = memoize(test)
    assert(mTest("A") == 0)
    assert(mTest("A") == 0)
    assert(mTest("B") == 1)
    assert(mTest("B") == 1)
    mTest.clearCache("B")
    assert(mTest("A") == 0)
    assert(mTest("B") == 2)
    mTest.clearCache()
    assert(mTest("A") == 3)
    assert(mTest("B") == 4)
  }

  test("TimeIt") {

    def test1(): Int =
      var i = 0
      while (i < 10000)
        i += 1
      i

    def test2(): Unit =
      var i = 0
      while (i < 10000)
        i += 1

    def test3(j: Int): Int =
      var i = 0
      while (i < j)
        i += 1
      i

    def test4(j: Int): Unit =
      var i = 0
      while (i < j)
        i += 1

    lazy val tTest1 = timeIt[Int](test1)
    assert(tTest1.durationsInNS.isEmpty)
    tTest1()
    assert(tTest1.durationsInNS.nonEmpty)
    tTest1()
    assert(tTest1.durationsInNS.nonEmpty)

    lazy val tTest2 = timeIt[Unit](test2)
    assert(tTest2.durationsInNS.isEmpty)
    tTest2()
    assert(tTest2.durationsInNS.nonEmpty)
    tTest2()
    assert(tTest2.durationsInNS.nonEmpty)

    lazy val tTest3 = timeIt(test3)
    assert(tTest3.durationsInNS.isEmpty)
    tTest3(10000)
    assert(tTest3.durationsInNS.nonEmpty)
    tTest3(10000)
    assert(tTest3.durationsInNS.nonEmpty)

    lazy val tTest4 = timeIt(test4)
    assert(tTest4.durationsInNS.isEmpty)
    tTest4(10000)
    assert(tTest4.durationsInNS.nonEmpty)
    tTest4(10000)
    assert(tTest4.durationsInNS.nonEmpty)
  }
