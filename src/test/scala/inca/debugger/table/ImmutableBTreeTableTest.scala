package inca.debugger.table

import inca.debugger.table.indexing.IndexCover
import inca.debugger.ScalaValue
import inca.debugger.Value
import inca.util.FilesUtil
import inca.util.TimeTracker
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable

class ImmutableBTreeTableTest extends AnyFunSuite {
  def tuple(args: Any*): Seq[Value] = args.map(ScalaValue.apply)

  def constructTable(
      cols: Seq[String],
      elems: Seq[Seq[Value]],
      _indexCovers: Set[IndexCover] = Set()
    ): ImmutableTable[Value] = {
    val indexCovers =
      if (_indexCovers.isEmpty)
        Set(IndexCover(cols))
      else _indexCovers
    ImmutableBTreeTable[Value](cols, elems, indexCovers)
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
        tuple("andre", 31, true),
        tuple("isa", 27, false),
        tuple("andre", 28, false),
        tuple("patrick", 27, true)
      ),
      indexCovers.toSet)

    assert(table.contains(tuple("andre", 31, true), indexCovers.head))
    assert(table.contains(tuple("andre", 31, true), indexCovers(1)))
    assert(table.contains(tuple("andre", 31, true), indexCovers(2)))
    assert(!table.contains(tuple("andre", 31, false), indexCovers.head))
    assert(!table.contains(tuple("andre", 31, false), indexCovers(1)))
    assert(!table.contains(tuple("andre", 31, false), indexCovers(2)))

    assertResult(tuple("andre", "andre", "isa", "patrick"))(
      table.entries(indexCovers.head).map(_.head))
    assertResult(tuple("isa", "patrick", "andre", "andre"))(
      table.entries(indexCovers(1)).map(_.head))
    assertResult(tuple("isa", "andre", "patrick", "andre"))(
      table.entries(indexCovers(2)).map(_.head))
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
        tuple("andre", 31, true),
        tuple("isa", 27, false),
        tuple("andre", 28, false),
        tuple("patrick", 27, true)
      ),
      indexCovers.toSet)

    val table2 = constructTable(
      Seq("name", "age", "m"),
      Seq(
        tuple("lukas", 27, true),
        tuple("fabio", 26, true)
      ),
      indexCovers.toSet)

    val union = table1.union(table2)

    assertResult(Seq("name", "age", "m"))(union.columns)
    assertResult(tuple("andre", "andre", "fabio", "isa", "lukas", "patrick"))(
      union.entries(indexCovers.head).map(_.head))
  }

  test("union with empty table on lhs") {
    val indexCovers = Seq(
      IndexCover(Seq("name", "age", "m")),
      IndexCover(Seq("age", "name", "m")),
      IndexCover(Seq("m", "age", "name"))
    )
    val table1 = constructTable(Seq("name", "age", "m"), Seq(), indexCovers.toSet)

    val table2 = constructTable(
      Seq("name", "age", "m"),
      Seq(
        tuple("lukas", 27, true),
        tuple("fabio", 26, true)
      ),
      indexCovers.toSet)

    val union = table1.union(table2)

    assertResult(Seq("name", "age", "m"))(union.columns)
    assertResult(table2.entries)(union.entries)
  }

  test("union with empty table on rhs") {
    val indexCovers = Seq(
      IndexCover(Seq("name", "age", "m")),
      IndexCover(Seq("age", "name", "m")),
      IndexCover(Seq("m", "age", "name"))
    )
    val table1 = constructTable(
      Seq("name", "age", "m"),
      Seq(
        tuple("lukas", 27, true),
        tuple("fabio", 26, true)
      ),
      indexCovers.toSet)

    val table2 = constructTable(Seq("name", "age", "m"), Seq(), indexCovers.toSet)

    val union = table1.union(table2)

    assertResult(Seq("name", "age", "m"))(union.columns)
    assertResult(table1.entries)(union.entries)
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
        tuple("andre", 31, true),
        tuple("isa", 27, false),
        tuple("andre", 28, false),
        tuple("patrick", 27, true)
      ),
      indexCovers.toSet)

    val table2 = constructTable(
      Seq("name", "age", "m"),
      Seq(
        tuple("andre", 28, false),
        tuple("fabio", 26, true)
      ),
      indexCovers.toSet)

    val diff = table1.diff(table2)

    assertResult(Seq("name", "age", "m"))(diff.columns)
    assertResult(tuple("andre", "isa", "patrick"))(diff.entries(indexCovers.head).map(_.head))
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
        tuple("andre", 31, true),
        tuple("isa", 27, false),
        tuple("andre", 28, false),
        tuple("patrick", 27, true)
      ),
      indexCovers.toSet)

    val newIndexCover = IndexCover(Seq("name", "m"))
    val projection = table1.project(newIndexCover.order)

    assertResult(Seq("name", "m"))(projection.columns)
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
        tuple("andre", 31, true),
        tuple("isa", 27, false),
        tuple("andre", 28, false),
        tuple("patrick", 27, true)
      ),
      indexCovers.toSet)

    val selection = table1.select { tuple =>
      tuple.head == ScalaValue("andre")
    }

    assertResult(Seq("name", "age", "m"))(selection.columns)
    assertResult(Seq(tuple("andre", 28, false), tuple("andre", 31, true)))(
      selection.entries(indexCovers.head))
  }

  test("simple join") {
    val table1 =
      constructTable(
        Seq("x", "y"),
        Seq(tuple(1, 2), tuple(2, 3), tuple(1, 3), tuple(2, 4), tuple(4, 2)))
    val table2 =
      constructTable(Seq("y", "z"), Seq(tuple(2, 6), tuple(2, 7), tuple(1, 3), tuple(3, 10)))
    val joined = table1.join(table2)
    val expectedEntries = Seq(
      tuple(1, 2, 6),
      tuple(1, 2, 7),
      tuple(1, 3, 10),
      tuple(2, 3, 10),
      tuple(4, 2, 6),
      tuple(4, 2, 7))
    assertResult(Seq("x", "y", "z"))(joined.columns)
    assertResult(expectedEntries)(joined.entries)
  }

  test("join with unit table on lhs") {
    val table1 =
      constructTable(Seq(), Seq(tuple()))
    val table2 =
      constructTable(Seq("x", "y"), Seq(tuple(2, 6), tuple(2, 7), tuple(1, 3), tuple(3, 10)))
    val joined = table1.join(table2)
    val expectedEntries = Seq(tuple(1, 3), tuple(2, 6), tuple(2, 7), tuple(3, 10))
    assertResult(Seq("x", "y"))(joined.columns)
    assertResult(expectedEntries)(joined.entries)
  }

  test("join with unit table on rhs") {
    val table1 =
      constructTable(Seq("x", "y"), Seq(tuple(2, 6), tuple(2, 7), tuple(1, 3), tuple(3, 10)))
    val table2 =
      constructTable(Seq(), Seq(tuple()))
    val joined = table1.join(table2)
    val expectedEntries = Seq(tuple(1, 3), tuple(2, 6), tuple(2, 7), tuple(3, 10))
    assertResult(Seq("x", "y"))(joined.columns)
    assertResult(expectedEntries)(joined.entries)
  }

  test("join with empty table on lhs") {
    val table1 =
      constructTable(Seq("y", "z"), Seq())
    val table2 =
      constructTable(Seq("x", "y"), Seq(tuple(2, 6), tuple(2, 7), tuple(1, 3), tuple(3, 10)))
    val joined = table1.join(table2)
    assertResult(Seq("y", "z", "x"))(joined.columns)
    assertResult(Seq())(joined.entries)
  }

  test("join with empty table on rhs") {
    val table1 =
      constructTable(Seq("x", "y"), Seq(tuple(2, 6), tuple(2, 7), tuple(1, 3), tuple(3, 10)))
    val table2 =
      constructTable(Seq("y", "z"), Seq())
    val joined = table1.join(table2)
    assertResult(Seq("x", "y", "z"))(joined.columns)
    assertResult(Seq())(joined.entries)
  }

  test("join with tables that have no common columns") {
    val table1 =
      constructTable(Seq("x", "y"), Seq(tuple(2, 6), tuple(2, 7), tuple(1, 3), tuple(3, 10)))
    val table2 =
      constructTable(Seq("z"), Seq(tuple(4)))
    val joined = table1.join(table2)
    val expectedEntries = Seq(tuple(1, 3, 4), tuple(2, 6, 4), tuple(2, 7, 4), tuple(3, 10, 4))
    assertResult(Seq("x", "y", "z"))(joined.columns)
    assertResult(expectedEntries)(joined.entries)
  }

  test("simple projectAndRename") {
    val indexCovers = Seq(
      IndexCover(Seq("name", "age", "m")),
      IndexCover(Seq("age", "name", "m")),
      IndexCover(Seq("m", "age", "name"))
    )
    val table1 = constructTable(
      Seq("name", "age", "m"),
      Seq(
        tuple("andre", 31, true),
        tuple("isa", 27, false),
        tuple("andre", 28, false),
        tuple("patrick", 27, true)
      ),
      indexCovers.toSet)

    val newIndexCover = IndexCover(Seq("name1", "male"))
    val subst = Map("name" -> "name1", "m" -> "male")
    val projection = table1.projectAndRename(subst)

    assertResult(Seq("name1", "male"))(projection.columns)
    assertResult(
      Seq(
        tuple("andre", false),
        tuple("andre", true),
        tuple("isa", false),
        tuple("patrick", true)))(projection.entries(newIndexCover))
  }

  test("entries of named tuple") {
    val table1 =
      constructTable(Seq("x", "y"), Seq(tuple(1, 2), tuple(1, 4), tuple(1, 5)))
    val entries = table1.entries(Map("x" -> ScalaValue(1)).toSeq)
    assertResult(table1.entries)(entries)
  }

  test("contains named tuple") {
    val table1 =
      constructTable(Seq("x", "y"), Seq(tuple(1, 2), tuple(1, 4), tuple(1, 5)))
    val check = table1.contains(Map("x" -> ScalaValue(1), "y" -> ScalaValue(2)).toSeq)
    assert(check)
  }

  test("contains named tuple (other ordering)") {
    val table1 =
      constructTable(Seq("x", "y"), Seq(tuple(1, 2), tuple(1, 4), tuple(1, 5)))
    val check = table1.contains(Map("y" -> ScalaValue(2), "x" -> ScalaValue(1)).toSeq)
    assert(check)
  }

  test("join where rhs is a projected table of the lhs") {
    val table1 =
      constructTable(Seq("x", "y"), Seq(tuple(1, 2), tuple(1, 4), tuple(1, 5)))
    val table2 =
      constructTable(Seq("x"), Seq(tuple(1)))
    val joined = table1.join(table2)
    val expectedEntries = Seq(tuple(1, 2), tuple(1, 4), tuple(1, 5))
    assertResult(Seq("x", "y"))(joined.columns)
    assertResult(expectedEntries)(joined.entries)
  }

  test("join where lhs is a projected table of the rhs") {
    val table1 =
      constructTable(Seq("x"), Seq(tuple(1)))
    val table2 =
      constructTable(Seq("x", "y"), Seq(tuple(1, 2), tuple(1, 4), tuple(1, 5)))
    val joined = table1.join(table2)
    val expectedEntries = Seq(tuple(1, 2), tuple(1, 4), tuple(1, 5))
    assertResult(Seq("x", "y"))(joined.columns)
    assertResult(expectedEntries)(joined.entries)
  }

  def buildTable(path: String, indexCover: Set[IndexCover] = Set()): ImmutableTable[Value] = {
    val entries: mutable.ListBuffer[Seq[Value]] = mutable.ListBuffer()
    FilesUtil.foreachFileLine(path) { line =>
      val elements = line.split("""\t\s?""")
      entries += elements.map(ScalaValue)
    }
    val cols = entries.head.map { case ScalaValue(s: String) => s }
    constructTable(cols, entries.tail.toSeq, indexCover)
  }

  test("real world anti-join") {
    // lhs has no index cover
    // right has index cover simplename, descriptor, type
    val t1 = buildTable("src/test/resources/table/antijoin-table1.txt")
    val t2 = buildTable(
      "src/test/resources/table/antijoin-table2.txt",
      Set(IndexCover(Seq("simplename", "descriptor", "type"))))
    val res = t1.antiJoin(t2)
    assert(true)
  }

  test("real world join") {
    val t1 = buildTable(
      "src/test/resources/table/join1-table1.txt",
      Set(IndexCover(Seq("type", "simplename", "descriptor"))))
    val t2 = buildTable(
      "src/test/resources/table/join1-table2.txt",
      Set(IndexCover(Seq("method", "descriptor")))) = TimeTracker.measure("JOIN", () => t1.join(t2))
    assert(true)
  }

  test("real world join 2") {
    val t1 =
      buildTable("src/test/resources/table/join2-table1.txt", Set(IndexCover(Seq("inmethod"))))
    val t2 =
      buildTable("src/test/resources/table/join2-table2.txt", Set(IndexCover(Seq("inmethod"))))
    val sameCols = t2.columns.filter(t1.columns.contains)
    TimeTracker.measure("JOIN", () => t1.join(t2))
    assert(true)
  }
}
