package inca.examples

import inca.Executor
import inca.runtime.context.LanguageMetaInfo
import org.scalatest.funsuite.AnyFunSuite
import truechange.{JavaLitType, ListType, SortType}
import truediff.macros.diffable

import scala.collection.immutable.MultiDict


class Graph extends AnyFunSuite {

  // language definition (data model as case classes)
  @diffable
  case class Graph(nodes: List[Node], edges: List[Edge])
  @diffable
  case class Node(name: String)
  @diffable
  case class Edge(from: Node, to: Node)


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
        (edgeTag->"from") -> nodeType,
        (edgeTag->"to") -> nodeType,
      ),
      Map(
        (nodeTag->"name") -> JavaLitType(classOf[java.lang.String]),
      )
    )
  }

  test("different functions for graphs trees") {
    val code =
     s"""module GraphAnalyses
        |
        |def directNeighbor(from: $nodeTag): $nodeTag = {
        |  val graph = from.parent:$graphTag
        |  val edge = graph.edges.children:$edgeTag
        |  assert edge.from.name == from.name
        |  yield edge.to
        |}
        |""".stripMargin
      ""

    val loaded = Executor.loadAnalysis(code, languageMetaInfo)

    val tree = Graph(List(Node("a"), Node("b"), Node("c")), List(Edge(Node("a"), Node("b")), Edge(Node("b"), Node("c"))))
    val res1 = loaded.execute(tree, "directNeighbor")
    res1.foreach(println)
  }
}
