package inca.util

import inca.util.datastructures.Graph
import org.scalatest.funsuite.AnyFunSuite

class GraphTest extends AnyFunSuite:
  class SimpleGraph extends Graph[String, String] {
    override def cloneGraph(): Graph[String, String] = throw new UnsupportedOperationException("Not implemented")

    override def nodeToGraphViz(n: String): String = namify(n)

    override def edgeGraphVizAttributes(from: String, to: String, info: String): String = "color=black"

    override def nodeGraphVizAttributes(from: String): String = "fillcolor=black, style=filled, fontcolor=white"
  }

  /**
   * A → B
   * ↓ ↖ ↓
   * C → D
   */
  test("Test Scc - but no cycle with B and C") {
    val g = new SimpleGraph
    g.addNode("A")
    g.addNode("B")
    g.addNode("C")
    g.addNode("D")
    g.addEdge("A", "B", "AB")
    g.addEdge("B", "D", "BD")
    g.addEdge("A", "C", "AC")
    g.addEdge("C", "D", "CD")
    g.addEdge("D", "A", "DA")

    val res = g.stronglyConnectedComponents.map(_.toSet).toSet
    assertResult(Set(
      Set("A", "B", "C", "D")
    ))(res)

    //println(g.stronglyConnectedComponentsWithInfo)
  }

  /**
   * A → B
   * ↖ ↓
   * E ←→ D ← C
   */
  test("Multiple SCCs") {
    val g = new SimpleGraph
    g.addNode("A")
    g.addNode("B")
    g.addNode("C")
    g.addNode("D")
    g.addNode("E")
    g.addEdge("A", "B", "AB")
    g.addEdge("B", "C", "BC")
    g.addEdge("C", "A", "CA")
    g.addEdge("C", "D", "CD")
    g.addEdge("D", "E", "DE")
    g.addEdge("E", "D", "ED")

    val res = g.stronglyConnectedComponents.map(_.toSet).toSet
    assertResult(Set(
      Set("A", "B", "C"),
      Set("D", "E"),
    ))(res)
  }