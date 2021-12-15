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
@diffable case class Graph(nodes: List[Node], edges: List[Edge], edges2: List[Edge])
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
        |
        |def edges(from: String): String = {
        |  vals g <- $graphTag
        |  foreach e in g.edges {
        |    assert e.from == from
        |    yield e.to
        |  }
        |}
        |
        |
        |def edges2(from: String): String = {
        |  vals g <- $graphTag
        |  foreach e in g.edges2 {
        |    assert e.from == from
        |    yield e.to
        |  }
        |}
        |
        |
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
        |@main
        |def paths_u(from: String, to: String): Unit = {
        |  assert to == edges(from)
        |} union {
        |  val inbetween = edges(from)
        |  assert def paths_u(inbetween, to)
        |}
        |@main
        |def paths2(from: String, to: String): Unit = {
        |  assert undef paths_u(from, to)
        |  assert to == edges2(from)
        |} union {
        |  val inbetween = edges2(from)
        |  assert undef paths_u(from, to)
        |  assert def paths2(inbetween, to)
        |}
        |""".stripMargin

    val loaded = ConstraintExecutor.loadAnalysis(code, ConstraintOptions())
    //val loaded = ConstraintExecutor.loadAnalysis(code, ConstraintOptions().withTransformations(FunctionalOptions.defaultTransformations))

    println(loaded.compiled.psystemSource)

//    val tree = Graph(List(Node("a"), Node("b"), Node("c")), List(Edge("a", "b"), Edge("b", "c")), List())
//    val res1 = loaded.execute(tree, "paths", Tuples.flatTupleOf("a", "c"))
//    res1.foreach(println)
//
//    val tree2 = Graph(List(Node("a"), Node("b"), Node("c"), Node("d")), List(Edge("a", "b"), Edge("b", "c"), Edge("b", "d"), Edge("c", "a")), List())
//    val res2 = loaded.update(tree2, "paths_u", Tuples.flatTupleOf("b"))
//    println("updated")
//    res2.foreach(println)

    val tree3 = Graph(List(Node("a"), Node("b"), Node("c"), Node("d"), Node("e"), Node("f")),
                      List(Edge("a", "b"), Edge("b", "c"), Edge("b", "d"), Edge("c", "a")),
                      List(Edge("d", "e"), Edge("e", "c"), Edge("c", "d"), Edge("e", "f")))
    val res3 = loaded.execute(tree3, "paths2")
    loaded.output("paths_u").foreach(println)
    println("updated")
    res3.foreach(println)
  }
}
