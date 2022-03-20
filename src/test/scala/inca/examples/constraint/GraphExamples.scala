package inca.examples.constraint

import inca.frontend.constraint.compiler.ConstraintOptions
import inca.frontend.constraint.executor.ConstraintExecutor
import inca.frontend.functional.compiler.FunctionalOptions
import inca.runtime.context.DataModel
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
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
        |@main
        |def edges(from: String): String = {
        |  vals e <- $edgeTag
        |  assert e.from == from
        |  yield e.to
        |}
        |
        |@main
        |def blacklist(node: String): Unit = {
        |  assert node == "z"
        |}
        |
        |@main
        |def paths(from: String): String = {
        |  yield edges(from)
        |} union {
        |  val inbetween = edges(from)
        |  // assert undef blacklist(inbetween)
        |  yield paths(inbetween)
        |}
        |
        |""".stripMargin

//    val loaded = ConstraintExecutor.loadAnalysis(code, ConstraintOptions())
    val loaded = ConstraintExecutor.loadAnalysis(
      code,
      ConstraintOptions().withTransformations(FunctionalOptions.defaultTransformations)
    )

    println(loaded.compiled.optimized)

    val tree = Graph(List(Node("a"), Node("b"), Node("c")), List(Edge("a", "b"), Edge("b", "c")))
    val res1 = loaded.execute(tree, "paths", Tuples.flatTupleOf("a"))
    res1.foreach(println)

    val tree2 = Graph(
      List(Node("a"), Node("b"), Node("c"), Node("d")),
      List(Edge("a", "b"), Edge("b", "c"), Edge("c", "d"), Edge("c", "a"))
    )
    val res2 = loaded.update(tree2, "paths", Tuples.flatTupleOf("a"))
    println("updated")
    res2.foreach(println)
  }
}
