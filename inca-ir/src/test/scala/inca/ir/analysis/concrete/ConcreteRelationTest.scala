package inca.ir.analysis.concrete

import inca.ir.analysis.base.values.ConcreteRelation
import org.scalatest.funsuite.AnyFunSuiteLike

class ConcreteRelationTest extends AnyFunSuiteLike:

  // Natural Join

  test("Natural Join - Unit Table x Table") {
    val table1 = ConcreteRelation(Seq(), Set(Seq[Int]()))
    val table2 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val result = table1.naturalJoin(table2)
    assert(result == table2)
  }

  test("Natural Join - Table x Unit Table") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq(), Set(Seq[Int]()))
    val result = table1.naturalJoin(table2)
    assert(result == table1)
  }

  test("Natural Join - Table x Table (single shared column)") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq("b", "c"), Set(Seq(2, 5), Seq(4, 6)))
    val result = table1.naturalJoin(table2)
    val expected = ConcreteRelation(Seq("a", "b", "c"), Set(Seq(1, 2, 5), Seq(3, 4, 6)))
    assert(result == expected)
  }

  test("Natural Join - Table x Table (no shared common columns)") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq("c", "d"), Set(Seq(5, 6), Seq(7, 8)))
    val result = table1.naturalJoin(table2)
    val expected = ConcreteRelation(Seq("a", "b", "c", "d"), Set(Seq(1, 2, 5, 6), Seq(1, 2, 7, 8), Seq(3, 4, 5, 6), Seq(3, 4, 7, 8)))
    assert(result == expected)
  }

  test("Natural Join - Table x Table (multiple shared columns 1)") {
    val table1 = ConcreteRelation(Seq("a", "b", "c"), Set(Seq(1, 2, 3), Seq(4, 5, 6)))
    val table2 = ConcreteRelation(Seq("b", "c", "d"), Set(Seq(2, 3, 4), Seq(5, 6, 7)))
    val result = table1.naturalJoin(table2)
    val expected = ConcreteRelation(Seq("a", "b", "c", "d"), Set(Seq(1, 2, 3, 4), Seq(4, 5, 6, 7)))
    assert(result == expected)
  }

  test("Natural Join - Table x Table (multiple shared columns 2)") {
    val table1 = ConcreteRelation(Seq("a", "b", "c"), Set(Seq(1, 2, 3), Seq(4, 5, -1)))
    val table2 = ConcreteRelation(Seq("b", "c", "d"), Set(Seq(2, 3, 4), Seq(5, 6, 7)))
    val result = table1.naturalJoin(table2)
    val expected = ConcreteRelation(Seq("a", "b", "c", "d"), Set(Seq(1, 2, 3, 4)))
    assert(result == expected)
  }


  // Union

  test("Union - Unit Table x Table") {
    val table1 = ConcreteRelation(Seq(), Set(Seq[Int]()))
    val table2 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    assertThrows[IllegalArgumentException] {
      table1.union(table2)
    }
  }

  test("Union - Table x Unit Table") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq(), Set(Seq[Int]()))
    assertThrows[IllegalArgumentException] {
      table1.union(table2)
    }
  }

  test("Union - Table x Table with Columns but no rows") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq("a", "b"), Set(Seq[Int]()))
    val result = table1.union(table2)
    assert(result == ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4), Seq())))
  }

  test("Union -  Table with Columns but no rows x Table") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq("a", "b"), Set(Seq[Int]()))
    val result = table2.union(table1)
    assert(result == ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4), Seq())))
  }

  test("Union - Table x Table (same columns )") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq("a", "b"), Set(Seq(5, 6), Seq(7, 8)))
    val result = table1.union(table2)
    val expected = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4), Seq(5, 6), Seq(7, 8)))
    assert(result == expected)
  }

  test("Union - Table x Table (same columns rearranged)") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq("b", "a"), Set(Seq(5, 6), Seq(7, 8)))
    val result = table1.union(table2)
    val expected = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4), Seq(6, 5), Seq(8, 7)))
    assert(result == expected)
  }

  test("Union - Table x Table (different columns | different size)") {
    val table1 = ConcreteRelation(Seq("a", "b", "c"), Set(Seq(1, 2, 3), Seq(4, 5, 6)))
    val table2 = ConcreteRelation(Seq("c", "d"), Set(Seq(5, 6), Seq(7, 8), Seq(9, 10)))
    assertThrows[IllegalArgumentException] {
      table1.union(table2)
    }
  }

  test("Union - Table x Table (different columns | same size)") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(4, 5)))
    val table2 = ConcreteRelation(Seq("c", "d"), Set(Seq(5, 6), Seq(7, 8), Seq(9, 10)))
    assertThrows[IllegalArgumentException] {
      table1.union(table2)
    }
  }

  // Cartesian

  /*test("Cartesian - Unit Table x Table") {
    val table1 = ConcreteRelation(Seq(), Set(Seq[Int]()))
    val table2 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val result = table1.cartesian(table2)
    assert(result == table2)
  }

  test("Cartesian - Table x Unit Table") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq(), Set(Seq[Int]()))
    val result = table1.cartesian(table2)
    assert(result == table1)
  }

  test("Cartesian - Table x Table (no shared columns)") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq("c", "d"), Set(Seq(5, 6), Seq(7, 8)))
    val result = table1.cartesian(table2)
    val expected = ConcreteRelation(Seq("a", "b", "c", "d"), Set(Seq(1, 2, 5, 6), Seq(1, 2, 7, 8), Seq(3, 4, 5, 6), Seq(3, 4, 7, 8)))
    assert(result == expected)
  }

  test("Cartesian - Table x Table (shared columns)") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq("b", "c"), Set(Seq(5, 6), Seq(7, 8)))
    assertThrows[IllegalArgumentException] {
      table1.cartesian(table2)
    }
  }*/

  // Anti Join

  test("Anti Join - Unit Table x Table") {
    val table1 = ConcreteRelation(Seq(), Set(Seq[Int]()))
    val table2 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    assertThrows[IllegalArgumentException] {
      table1.antiJoin(table2)
    }
  }

  test("Anti Join - Table x Unit Table") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq(), Set(Seq[Int]()))
    assertThrows[IllegalArgumentException] {
      table1.antiJoin(table2)
    }
  }

  test("Anti Join - Table x Table (no shared columns)") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq("c", "d"), Set(Seq(5, 6), Seq(7, 8)))
    assertThrows[IllegalArgumentException] {
      table1.antiJoin(table2)
    }
  }

  test("Anti Join - Table x Table (shared columns | same values)") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq("b", "c"), Set(Seq(2, 3), Seq(4, 5)))
    val result = table1.antiJoin(table2)
    assert(result.rows.isEmpty)
  }

  test("Anti Join - Table x Table (shared columns | different values)") {
    val table1 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, -2), Seq(3, 4)))
    val table2 = ConcreteRelation(Seq("b", "c"), Set(Seq(2, 3), Seq(4, 5), Seq(6, 7)))
    val result = table1.antiJoin(table2)
    val expected = ConcreteRelation(Seq("a", "b"), Set(Seq(1, -2)))
    assert(result == expected)
  }

  /*test("Anti Join - Empty Table x Table") {
    val table1 = ConcreteRelation(Seq(), Set[Seq[Int]]())
    val table2 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val result = table1.antiJoin(table2)
    assert(result == table1)
  }

  test("Anti Join - Table x Empty Table") {
    val table1 = ConcreteRelation(Seq(), Set[Seq[Int]]())
    val table2 = ConcreteRelation(Seq("a", "b"), Set(Seq(1, 2), Seq(3, 4)))
    val result = table2.antiJoin(table1)
    assert(result == table1)
  }*/