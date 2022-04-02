package inca.debugger.table

import inca.debugger.table.indexing.IndexCover
import inca.debugger.ScalaValue
import inca.debugger.Value
import org.scalatest.funsuite.AnyFunSuite

class ImmutableBTreeTableTest extends AnyFunSuite {
  def tuple(args: Any*): Seq[Value] = args.map(ScalaValue.apply)

  def constructTable(
      cols: Seq[String],
      elems: Seq[Seq[Any]],
      _indexCovers: Set[IndexCover] = Set()
    ): ImmutableTable[Value] = {
    val indexCovers =
      if (_indexCovers.isEmpty)
        Set(IndexCover(cols))
      else _indexCovers
    ImmutableBTreeTable[Value](cols, 256, elems.map(tuple), indexCovers)
  }

  test("simple contains test") {
    val indexCovers = Seq(
      IndexCover(Seq("name", "age", "m")),
      IndexCover(Seq("age", "name", "m")),
      IndexCover(Seq("m", "age", "name"))
    )
    val table = constructTable(
      Seq("name", "age", "m"),
      Seq(
        Seq("andre", 31, true),
        Seq("isa", 27, false),
        Seq("andre", 28, false),
        Seq("patrick", 27, true)
      ),
      indexCovers.toSet)

    assert(table.contains(tuple("andre", 31, true), indexCovers.head))
    assert(table.contains(tuple("andre", 31, true), indexCovers(1)))
    assert(table.contains(tuple("andre", 31, true), indexCovers(2)))
    assert(!table.contains(tuple("andre", 31, false), indexCovers.head))
    assert(!table.contains(tuple("andre", 31, false), indexCovers(1)))
    assert(!table.contains(tuple("andre", 31, false), indexCovers(2)))

    assertResult(tuple("andre", "andre", "isa", "patrick"))(
      table.entries(indexCovers.head).toSeq.map(_.head))
    assertResult(tuple("isa", "patrick", "andre", "andre"))(
      table.entries(indexCovers(1)).toSeq.map(_.head))
    assertResult(tuple("isa", "andre", "patrick", "andre"))(
      table.entries(indexCovers(2)).toSeq.map(_.head))
  }

  test("simple union") {
    val indexCovers = Seq(
      IndexCover(Seq("name", "age", "m")),
      IndexCover(Seq("age", "name", "m")),
      IndexCover(Seq("m", "age", "name"))
    )
    val table1 = constructTable(
      Seq("name", "age", "m"),
      Seq(
        Seq("andre", 31, true),
        Seq("isa", 27, false),
        Seq("andre", 28, false),
        Seq("patrick", 27, true)
      ),
      indexCovers.toSet)

    val table2 = constructTable(
      Seq("name", "age", "m"),
      Seq(
        Seq("lukas", 27, true),
        Seq("fabio", 26, true)
      ),
      indexCovers.toSet)

    val union = table1.union(table2)

    assertResult(tuple("andre", "andre", "fabio", "isa", "lukas", "patrick"))(
      union.entries(indexCovers.head).toSeq.map(_.head))
  }

  test("simple diff") {
    val indexCovers = Seq(
      IndexCover(Seq("name", "age", "m")),
      IndexCover(Seq("age", "name", "m")),
      IndexCover(Seq("m", "age", "name"))
    )
    val table1 = constructTable(
      Seq("name", "age", "m"),
      Seq(
        Seq("andre", 31, true),
        Seq("isa", 27, false),
        Seq("andre", 28, false),
        Seq("patrick", 27, true)
      ),
      indexCovers.toSet)

    val table2 = constructTable(
      Seq("name", "age", "m"),
      Seq(
        Seq("andre", 28, false),
        Seq("fabio", 26, true)
      ),
      indexCovers.toSet)

    val diff = table1.diff(table2)

    assertResult(tuple("andre", "isa", "patrick"))(diff.entries(indexCovers.head).toSeq.map(_.head))
  }

  test("simple project") {
    val indexCovers = Seq(
      IndexCover(Seq("name", "age", "m")),
      IndexCover(Seq("age", "name", "m")),
      IndexCover(Seq("m", "age", "name"))
    )
    val table1 = constructTable(
      Seq("name", "age", "m"),
      Seq(
        Seq("andre", 31, true),
        Seq("isa", 27, false),
        Seq("andre", 28, false),
        Seq("patrick", 27, true)
      ),
      indexCovers.toSet)

    val newIndexCover = IndexCover(Seq("name", "m"))
    val projection = table1.project(newIndexCover.order)

    assertResult(
      Seq(
        tuple("andre", false),
        tuple("andre", true),
        tuple("isa", false),
        tuple("patrick", true)))(projection.entries(newIndexCover))
  }

  test("simple select") {
    val indexCovers = Seq(
      IndexCover(Seq("name", "age", "m")),
      IndexCover(Seq("age", "name", "m")),
      IndexCover(Seq("m", "age", "name"))
    )
    val table1 = constructTable(
      Seq("name", "age", "m"),
      Seq(
        Seq("andre", 31, true),
        Seq("isa", 27, false),
        Seq("andre", 28, false),
        Seq("patrick", 27, true)
      ),
      indexCovers.toSet)

    val newIndexCover = IndexCover(Seq("name", "m"))
    val selection = table1.select { tuple =>
      tuple.head == ScalaValue("andre")
    }

    assertResult(Seq(tuple("andre", 28, false), tuple("andre", 31, true)))(
      selection.entries(indexCovers.head))
  }

  test("simple join") {
    val table1 =
      constructTable(Seq("x", "y"), Seq(Seq(1, 2), Seq(2, 3), Seq(1, 3), Seq(2, 4), Seq(4, 2)))
    val table2 =
      constructTable(Seq("y", "z"), Seq(Seq(2, 6), Seq(2, 7), Seq(1, 3), Seq(3, 10)))
    val joined = table1.join(table2)
    val expectedEntries = Seq(
      tuple(1, 2, 6),
      tuple(1, 2, 7),
      tuple(1, 3, 10),
      tuple(2, 3, 10),
      tuple(4, 2, 6),
      tuple(4, 2, 7))
    assertResult(expectedEntries)(joined.entries)
  }
}
