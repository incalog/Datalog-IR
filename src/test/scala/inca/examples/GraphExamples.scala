package inca.examples

import inca.Executor
import inca.runtime.context.LanguageMetaInfo
import org.scalatest.funsuite.AnyFunSuite
import truechange.{JavaLitType, ListType, SortType}
import truediff.macros.diffable

import scala.collection.immutable.MultiDict

// language definition (data model as case classes)
// IMPORTANT needs to be a top-level definition
@diffable case class Graph(nodes: List[Node], edges: List[Edge])
@diffable case class Node(name: String)
@diffable case class Edge(from: String, to: String)

class GraphExamples extends AnyFunSuite {
  val graphTag = classOf[Graph].getCanonicalName
  val nodeTag = classOf[Node].getCanonicalName
  val edgeTag = classOf[Edge].getCanonicalName

  // meta information about data model
  val languageMetaInfo: LanguageMetaInfo = {
    val graphType = SortType(graphTag)
    val nodeType = SortType(nodeTag)
    val edgeType = SortType(edgeTag)
    new LanguageMetaInfo(
      MultiDict[SortType, SortType](),
      Map(
        (graphTag->"nodes") -> ListType(nodeType),
        (graphTag->"edges") -> ListType(edgeType),
      ),
      Map(
        (nodeTag->"name") -> JavaLitType(classOf[java.lang.String]),
        (edgeTag->"from") -> JavaLitType(classOf[java.lang.String]),
        (edgeTag->"to") -> JavaLitType(classOf[java.lang.String]),
      )
    )
  }

  test("different functions for graphs trees") {
    val code =
     s"""module GraphAnalyses
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
        |def inCycleByName(): String = {
        |  vals node <- $nodeTag
        |  assert def inCycle(node)
        |  yield node.name
        |}
        |""".stripMargin
      ""

    val loaded = Executor.loadAnalysis(code, languageMetaInfo)

    val tree = Graph(List(Node("a"), Node("b"), Node("c")), List(Edge("a", "b"), Edge("b", "c")))
    val res1 = loaded.execute(tree, "inCycleByName")
    res1.foreach(println)
    val tree2 = Graph(List(Node("a"), Node("b"), Node("c"), Node("d")), List(Edge("a", "b"), Edge("b", "c"), Edge("c", "d"), Edge("c", "a")))
    val res2 = loaded.update(tree2, "inCycleByName")
    println("updated")
    res2.foreach(println)
  }
}
