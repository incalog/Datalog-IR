package inca.backend.analyze

import scala.collection.mutable
import inca.backend.ir.Datalog._

trait Graph[N, E] {
  val nodes: mutable.Set[N] = mutable.Set()
  val edges: mutable.Map[N, Set[(N, E)]] = mutable.Map()

  def addEdge(from: N, to: N, info: E): Unit = {
    nodes += from
    nodes += to
    edges.get(from) match {
      case Some(old) =>
        edges += from -> (old + (to -> info))
      case None =>
        edges += from -> Set((to, info))
    }
  }

  def toGraphViz: String = {
    val sb = new StringBuilder()
    nodes.foreach { from =>
      edges.getOrElse(from, Nil).foreach { case (to, info) =>
        val edge = s"\t${nodeToGraphViz(from)} -> ${nodeToGraphViz(to)} [${edgeToGraphViz(from, to, info)}];\n"
        sb ++= edge
      }
    }

    s"""strict digraph {
       |  ${sb.toString()}
       |}
       |""".stripMargin
  }

  def edgeToGraphViz(from: N, to: N, info: E): String
  def nodeToGraphViz(n: N): String
}

class DependencyGraph extends Graph[Pattern, Boolean] {
  override def edgeToGraphViz(from: Pattern, to: Pattern, info: Boolean): String = if (info) "color=red" else "color=black"

  override def nodeToGraphViz(n: Pattern): String = n.name.replace("$", "_")
}
