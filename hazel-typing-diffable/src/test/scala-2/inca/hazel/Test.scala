package inca.hazel

import org.scalatest.funsuite.AnyFunSuiteLike
import edb._

class TestEdb extends AnyFunSuiteLike {

  test("Basic diffing") {
    val t1 = TAArrow(TANum(), TANum())
    val t2 = TAArrow(TABool(), TANum())
    val t3 = TAArrow(TAUnknown(), TANum())

    println(s"Loading $t1")
    val edits1 = t1.loadEdits
    edits1.print()
    println(s"Yielding ${t1.toStringWithURI}")
    println()

    println(s"Replace by $t2.to")
    val (edits12, newT2) = t1.compareTo(t2)
    edits12.print()
    println(s"Yielding ${newT2.toStringWithURI}")
    println()

    println(s"Replace by $t3")
    val (edits23, newT3) = newT2.compareTo(t3)
    edits23.print()
    println(s"Yielding ${newT3.toStringWithURI}")
    println()
  }
}