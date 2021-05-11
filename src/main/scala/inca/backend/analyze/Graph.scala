package inca.backend.analyze

import scala.collection.mutable
import inca.backend.ir.Datalog._

// based on LangComp Lab work of Saleh Oshaghi and Tomislav Pree
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

  // based on https://www.baeldung.com/cs/detecting-cycles-in-directed-graph
  private trait VisistedFlag
  private case object NotVisisted extends VisistedFlag
  private case object InStack extends VisistedFlag
  private case object Done extends VisistedFlag

  def cycles(): List[List[N]] = {
    val visited: mutable.Map[N, VisistedFlag] = mutable.Map()
    nodes.foreach { n => visited(n) = NotVisisted }

    var cycles: Set[List[N]] = Set()

    nodes.foreach { node =>
      if (visited(node) == NotVisisted) {
        val stack = mutable.Stack[N]()
        stack.push(node)
        visited(node) = InStack
        cycles = cycles ++ processDFSTree(stack, visited)
      }
    }
    cycles.toList
  }

  private def processDFSTree(stack: mutable.Stack[N], visited: mutable.Map[N, VisistedFlag]): Set[List[N]] = {
    var cycles: Set[List[N]] = Set()
    edges.getOrElse(stack.top, Set()).foreach { case (neighbor, e) =>
      if (visited(neighbor) == InStack) {
        cycles = cycles + determineCycle(stack, neighbor)
      } else if (visited(neighbor) == NotVisisted) {
        stack.push(neighbor)
        visited(neighbor) = InStack
        cycles = cycles ++ processDFSTree(stack, visited)
      }
    }
    visited(stack.top) = Done
    stack.pop()
    cycles
  }

  private def determineCycle(stack: mutable.Stack[N], node: N): List[N] = {
    val otherStack: mutable.Stack[N] = mutable.Stack()
    otherStack.push(stack.top)
    stack.pop()
    while (otherStack.top != node) {
      otherStack.push(stack.top)
      stack.pop
    }

    var cycle: List[N] = List()
    while (otherStack.nonEmpty) {
      cycle = otherStack.top +: cycle
      stack.push(otherStack.top)
      otherStack.pop
    }
    cycle
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

  protected def edgeToGraphViz(from: N, to: N, info: E): String
  protected def nodeToGraphViz(n: N): String
}

class DependencyGraph extends Graph[Pattern, Boolean] {
  override protected def edgeToGraphViz(from: Pattern, to: Pattern, info: Boolean): String = if (info) "color=red" else "color=black"

  override protected def nodeToGraphViz(n: Pattern): String = n.name.replace("$", "_")
}
