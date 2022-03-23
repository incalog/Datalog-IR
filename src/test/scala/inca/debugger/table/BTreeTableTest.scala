package inca.debugger.table

import inca.debugger.table.MutableTable.IndexOrder
import inca.debugger.ScalaValue
import inca.debugger.Value
import org.scalatest.funsuite.AnyFunSuite

class BTreeTableTest extends AnyFunSuite {
  def tuple(args: Any*): Seq[Value] = args.map(ScalaValue.apply)

  def constructTree(
      cols: Seq[String],
      elems: Seq[Seq[Any]],
      minDegree: Int = 256,
      index: Seq[IndexOrder] = Seq()
    ): BTreeTable[Value] = {
    val table = new BTreeTable[Value](cols, minDegree, index)
    elems.foreach { t =>
      table.insert(tuple(t: _*))
    }
    table
  }

  test("simple inserts") {
    val table = new BTreeTable[Value](Seq("name", "age", "m"), 2)
    table.insert(tuple("andre", 31, true))
    table.insert(tuple("isa", 27, false))
    table.insert(tuple("andre", 28, false))
    table.insert(tuple("patrick", 27, true))
    assertResult(tuple("andre", "andre", "isa", "patrick"))(table.entries.map(_.head))
  }

  test("inserts with non-standard index") {
    val table = new BTreeTable[Value](Seq("name", "age", "m"), 2, Seq(Seq(1, 2, 0)))
    table.insert(tuple("andre", 31, true))
    table.insert(tuple("isa", 27, true))
    table.insert(tuple("andre", 31, false))
    table.insert(tuple("patrick", 27, true))
    assertResult(tuple("isa", "patrick", "andre", "andre"))(table.entries.map(_.head))
  }

  test("test contains") {
    val table =
      constructTree(Seq("x", "y"), Seq(Seq(1, 2), Seq(2, 3), Seq(1, 3), Seq(2, 4), Seq(4, 2)))
    assert(table.contains(tuple(1, 2)))
    assert(table.contains(tuple(2, 3)))
    assert(table.contains(tuple(1, 3)))
    assert(table.contains(tuple(2, 4)))
    assert(table.contains(tuple(4, 2)))
    assert(!table.contains(tuple(2, 2)))
    assert(!table.contains(tuple(3, 2)))
    assert(!table.contains(tuple(4, 4)))
  }
}
