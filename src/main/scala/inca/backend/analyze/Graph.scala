package inca.backend.analyze

import scala.collection.mutable

// based on LangComp Lab work of Saleh Oshaghi and Tomislav Pree
trait Graph[N, E] {
  val nodes: mutable.Set[N] = mutable.Set()
  val edges: mutable.Map[N, Set[(N, E)]] = mutable.Map()

  def addNode(n: N): Unit = {
    nodes += n
  }

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
      sb ++= s"\t${nodeToGraphViz(from)} [${nodeGraphVizAttributes(from)}];\n"
      edges.getOrElse(from, Nil).foreach { case (to, info) =>
        val edge = s"\t${nodeToGraphViz(from)} -> ${nodeToGraphViz(to)} [${edgeGraphVizAttributes(from, to, info)}];\n"
        sb ++= edge
      }
    }

    s"""strict digraph {
       |  ${sb.toString()}
       |}
       |""".stripMargin
  }

  protected def nodeToGraphViz(n: N): String
  protected def edgeGraphVizAttributes(from: N, to: N, info: E): String
  protected def nodeGraphVizAttributes(from: N): String
}

