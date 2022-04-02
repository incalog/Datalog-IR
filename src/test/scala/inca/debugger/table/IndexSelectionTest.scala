package inca.debugger.table

import inca.debugger.table.indexing.IndexSelection
import inca.debugger.table.indexing.Search
import inca.debugger.table.indexing.SearchChain
import org.scalatest.funsuite.AnyFunSuite

class IndexSelectionTest extends AnyFunSuite {

  test("ignore subset search") {
    val searches = Set(
      Search.from("x"),
      Search.from("x", "y")
    )
    val min = IndexSelection.minIndex(searches)
    assertResult(SearchChain(Seq(Search.from("x", "y"))))(min)
  }

  test("ignore subset search (swapped)") {
    val searches = Set(
      Search.from("x", "y"),
      Search.from("x")
    )
    val min = IndexSelection.minIndex(searches)
    assertResult(SearchChain(Seq(Search.from("x", "y"))))(min)
  }

  test("ignore multiple subset searches") {
    val searches = Set(
      Search.from("x"),
      Search.from("x", "y"),
      Search.from("x", "y", "z")
    )
    val min = IndexSelection.minIndex(searches)
    assertResult(SearchChain(Seq(Search.from("x", "y", "z"))))(min)
  }

  test("example from paper") {
    val searches = Set(
      Search.from("x"),
      Search.from("x", "y"),
      Search.from("x", "z"),
      Search.from("x", "y", "z")
    )
    val min = IndexSelection.minIndex(searches)
    val expected = SearchChain(
      Seq(
        Search.from("x", "y", "z"),
        Search.from("x", "z")
      ))
    assertResult(expected)(min)
  }

  test("two different search chains") {
    val searches = Set(
      Search.from("x"),
      Search.from("x", "y"),
      Search.from("x", "z"),
      Search.from("x", "y", "z"),
      Search.from("x", "z", "y")
    )
    val min = IndexSelection.minIndex(searches)
    val expected = SearchChain(
      Seq(
        Search.from("x", "y", "z"),
        Search.from("x", "z", "y")
      ))
    assertResult(expected)(min)
  }

  test("two singleton searches") {
    val searches = Set(
      Search.from("x"),
      Search.from("z")
    )
    val min = IndexSelection.minIndex(searches)
    val expected = SearchChain(
      Seq(
        Search.from("x"),
        Search.from("z")
      ))
    assertResult(expected)(min)
  }

  test("swapped searches") {
    val searches = Set(
      Search.from("x", "y"),
      Search.from("y", "x")
    )
    val min = IndexSelection.minIndex(searches)
    val expected = SearchChain(
      Seq(
        Search.from("x", "y"),
        Search.from("y", "x")
      ))
    assertResult(expected)(min)
  }
}
