package inca.examples.constraint

import inca.frontend.constraint.executor.ConstraintExecutor
import inca.runtime.context.DataModel
import org.scalatest.funsuite.AnyFunSuite
import truediff.macros.diffable

// language definition (data model as case classes)
// IMPORTANT needs to be a top-level definition
@diffable case class Graph(nodes: List[Node], edges: List[Edge])
@diffable case class Node(name: String)
@diffable case class Edge(from: String, to: String)
object GraphModel {
  val model: DataModel = DataModel.from(Graph, Node, Edge)
}

class GraphExamples extends AnyFunSuite {

  val graphTag = classOf[Graph].getCanonicalName
  val nodeTag = classOf[Node].getCanonicalName
  val edgeTag = classOf[Edge].getCanonicalName

  test("different functions for graphs trees") {
    val code =
     s"""module GraphAnalyses
        |datamodel inca.examples.constraint.GraphModel.model
        |
        |def directNeighbor(from: $nodeTag): $nodeTag = {
        |  // from.parent points to nodes: List[Node]
        |  // from.parent.parent points to Graph
        |  val graph = from.parent.parent:$graphTag
        |  val edge = graph.edges.children:$edgeTag
        |  assert edge.from == from.name
        |  vals to <- $nodeTag
        |  assert edge.to == to.name
        |  yield to
        |}
        |
        |def path(from: $nodeTag): $nodeTag = {
        |  yield directNeighbor(from)
        |} union {
        |  val to = directNeighbor(from)
        |  yield path(to)
        |}
        |
        |def pathByName(): (String, String) = {
        |  // enumerates all nodes
        |  vals from <- $nodeTag
        |  val to = path(from)
        |  yield (from.name, to.name)
        |}
        |
        |def inCycle(node: $nodeTag): Unit = {
        |  assert path(node) == node
        |  yield unit
        |}
        |
        |@main
        |def inCycleByName(): String = {
        |  vals node <- $nodeTag
        |  assert def inCycle(node)
        |  yield node.name
        |}
        |""".stripMargin

    val loaded = ConstraintExecutor.loadAnalysis(code)

    val tree = Graph(List(Node("a"), Node("b"), Node("c")), List(Edge("a", "b"), Edge("b", "c")))
    val res1 = loaded.execute(tree, "inCycleByName")
//    res1.foreach(println)
    val tree2 = Graph(List(Node("a"), Node("b"), Node("c"), Node("d")), List(Edge("a", "b"), Edge("b", "c"), Edge("c", "d"), Edge("c", "a")))
    val res2 = loaded.update(tree2, "inCycleByName")
//    println("updated")
//    res2.foreach(println)
  }
}
