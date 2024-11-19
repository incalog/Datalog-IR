package inca.util.datastructures

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

  lazy val cyclesWithInfo: List[List[(N, E)]] =
    cycles.flatMap { cycle =>
      var prefixes: List[Vector[(N, E)]] = List(Vector())
      val reverse = cycle.reverse
      reverse.zip(reverse.tail).foreach { (from, to) =>
        val fromEdges = edges(from)
        val fromToEdges = fromEdges.filter(_._1 == to).map(_._2)
        prefixes =
          for (prefix <- prefixes; info <- fromToEdges) yield
            prefix :+ (from, info)
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

  lazy val topologicalSort: List[N] =
    val visited: mutable.Map[N, Boolean] = mutable.Map()
    val stack = mutable.Stack[N]()
    nodes.foreach { n => visited(n) = false }

    def topologicalSort(node: N): Unit = {
      visited(node) = true
      edges.getOrElse(node, Set()).foreach { case (neighbor, _) =>
        if (!visited(neighbor)) {
          topologicalSort(neighbor)
        }
      }
      stack.push(node)
    }

    nodes.foreach { node =>
      if (!visited(node)) {
        topologicalSort(node)
      }
    }
    stack.toList

  lazy val stronglyConnectedComponents: List[List[N]] =
    val stack = mutable.Stack[N]()
    val visited: mutable.Map[N, Boolean] = mutable.Map()
    nodes.foreach { n => visited(n) = false }
    val lowLink: mutable.Map[N, Int] = mutable.Map()
    val index: mutable.Map[N, Int] = mutable.Map()
    var indexCounter = 0
    var components: List[List[N]] = List()

    def strongConnect(node: N): Unit = {
      index(node) = indexCounter
      lowLink(node) = indexCounter
      indexCounter += 1
      stack.push(node)
      visited(node) = true

      edges.getOrElse(node, Set()).foreach { case (neighbor, _) =>
        if (!visited(neighbor)) {
          strongConnect(neighbor)
          lowLink(node) = Math.min(lowLink(node), lowLink(neighbor))
        } else if (stack.contains(neighbor)) {
          lowLink(node) = Math.min(lowLink(node), index(neighbor))
        }
      }

      if (lowLink(node) == index(node)) {
        var component: List[N] = List()
        var top = stack.pop()
        component = top +: component
        while (top != node) {
          top = stack.pop()
          component = top +: component
        }
        components = component +: components
      }
    }

    nodes.foreach { node =>
      if (!visited(node)) {
        strongConnect(node)
      }
    }
    components

  lazy val stronglyConnectedComponentsWithInfo: List[List[(N, E)]] =
    stronglyConnectedComponents.map { component =>
      component.flatMap { node =>
        edges.getOrElse(node, Set()).map { case (to, info) => (to, info) }
      }
    }

  def filter(nodeFilter: N => Boolean, edgeFilter: (N, N, E) => Boolean): Graph[N, E] =
    val g = cloneGraph()
    for (n <- g.nodes if !nodeFilter(n))
      g.removeNode(n)
    g.edges.mapValuesInPlace((from, tos) => tos.filter((to, info) => edgeFilter(from, to, info)))
    g

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

  protected def cloneGraph(): Graph[N, E]

  protected def nodeToGraphViz(n: N): String

  protected def edgeGraphVizAttributes(from: N, to: N, info: E): String

  protected def nodeGraphVizAttributes(from: N): String
}

