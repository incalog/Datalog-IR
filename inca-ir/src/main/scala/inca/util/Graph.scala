package inca.util

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// based on LangComp Lab work of Saleh Oshaghi and Tomislav Pree
trait Graph[N, E] {
  val nodes: mutable.Set[N] = mutable.Set()
  val edges: mutable.Map[N, Set[(N, E)]] = mutable.Map()

  def addNode(n: N): Unit =
    assert(!locked)
    nodes += n

  def addEdge(from: N, to: N, info: E): Unit =
    assert(!locked)
    nodes += from
    nodes += to
    edges.get(from) match {
      case Some(old) =>
        edges += from -> (old + (to -> info))
      case None =>
        edges += from -> Set((to, info))
    }

  def removeNode(node: N): Unit =
    assert(!locked)
    nodes -= node
    edges.remove(node)
    edges.mapValuesInPlace { (_, es) =>
      es.filter(_._1 != node)
    }

  lazy val outermostCycles: List[List[N]] = {
    def isNested(cycle: List[N]): Boolean = {
      cycles.filter(_ != cycle).exists { other =>
        cycle.forall(other.contains)
      }
    }
    val nestedCycles = cycles.filter(isNested)
    cycles.diff(nestedCycles)
  }

  // based on https://www.baeldung.com/cs/detecting-cycles-in-directed-graph
  private trait VisistedFlag
  private case object NotVisisted extends VisistedFlag
  private case object InStack extends VisistedFlag
  private case object Done extends VisistedFlag

  private var locked: Boolean = false
  lazy val cycles: List[List[N]] =
    locked = true
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

  lazy val cyclesWithInfo: List[List[(N,E)]] =
    cycles.flatMap { cycle =>
      var prefixes: List[Vector[(N,E)]] = List(Vector())
      val reverse = cycle.reverse
      reverse.zip(reverse.tail).foreach { (from, to) =>
        val fromEdges = edges(from)
        val fromToEdges = fromEdges.filter(_._1 == to).map(_._2)
        prefixes =
          for (prefix <- prefixes; info <- fromToEdges) yield
            prefix :+ (from,info)
      }
      val last = reverse.last
      val loopEdges = edges(last).filter(_._1 == reverse.head).map(_._2)
      prefixes =
        for (prefix <- prefixes; info <- loopEdges) yield
          prefix :+ (last, info)
      prefixes.map(_.toList)
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

  def namify(s: String): String = s match
    case "edge" => "edge_"
    case _ => s.replace("$", "_")

  protected def nodeToGraphViz(n: N): String
  protected def edgeGraphVizAttributes(from: N, to: N, info: E): String
  protected def nodeGraphVizAttributes(from: N): String
}

