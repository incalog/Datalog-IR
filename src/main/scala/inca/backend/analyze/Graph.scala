package inca.backend.analyze

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

  def removeNode(node: N): Unit = {
    nodes -= node
    edges.remove(node)
    edges.mapValuesInPlace { (_, es) =>
      es.filter(_._1 != node)
    }
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

  lazy val cycles: List[List[N]] = {
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
    edges.getOrElse(stack.top, Set()).foreach { case (neighbor, _) =>
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

  def transitvelyReachable(from: N): Set[N] = {
    val nodeInfo = stronglyconnectedComponentOfNode(from)
    val nodes = mutable.Set.from(nodeInfo.component)
    for (comp <- nodeInfo.uses)
      nodes ++= comp
    nodes.toSet
  }

  def inSameStronglyConnectedCompontent(n1: N, n2: N): Boolean = stronglyconnectedComponentOfNode(n1).component.contains(n2)

  case class NodeComponentInfo(component: Set[N], uses: List[Set[N]], usedBy: List[Set[N]])

  lazy val stronglyconnectedComponentOfNode: Map[N, NodeComponentInfo] = {
    val nodes: mutable.Map[N, NodeComponentInfo] = mutable.Map()
    var usedBy = stronglyConnectedComponentsNoDemand
    var uses = List[Set[N]]()
    while (usedBy.nonEmpty) {
      val comp = usedBy.head
      usedBy = usedBy.tail
      for (node <- comp)
        nodes += node -> NodeComponentInfo(comp, uses, usedBy)
      uses = comp :: uses
    }
    nodes.toMap
  }

  lazy val stronglyConnectedComponentsNoDemand: List[Set[N]] = stronglyConnectedComponents(false)

  lazy val stronglyConnectedComponentsWithDemand: List[Set[N]] = stronglyConnectedComponents()


  // based on https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
  def stronglyConnectedComponents(withDemand: Boolean = true): List[Set[N]] = {

    val consideredGraph = getConsideredNodes(withDemand)
    val (consideredNodes, consideredEdges) = consideredGraph

    val visited = mutable.Map[N, VisistedFlag]()
    consideredNodes.foreach { n => visited(n) = NotVisisted }

    var currentIndex = 0

    val lowlink = mutable.Map[N, Int]()
    val index = mutable.Map[N, Int]()
    val stack = mutable.Stack[N]()
    val components = ListBuffer[Set[N]]()

    consideredNodes.foreach { node =>
      if (!index.contains(node))
        currentIndex = strongConnect(node, currentIndex, consideredEdges, index, lowlink, visited, stack, components)
    }

    components.toList
  }

  private def strongConnect(node: N, currentIndex: Int, edges: Map[N, Set[(N, E)]],
                            index: mutable.Map[N, Int], lowlink: mutable.Map[N, Int], visited: mutable.Map[N, VisistedFlag],
                            stack: mutable.Stack[N], components: ListBuffer[Set[N]]): Int = {
    lowlink(node) = currentIndex
    index(node) = currentIndex
    var i = currentIndex + 1
    stack.push(node)
    visited(node) = InStack

    edges.getOrElse(node, Set()).foreach { edge =>
      val neighbor = edge._1
      index.get(neighbor) match {
        case Some(_) =>
          if (visited(neighbor) == InStack)
            lowlink(node) = lowlink(node).min(index(neighbor))
        case None =>
          i = strongConnect(neighbor, i, edges, index, lowlink, visited, stack, components)
          lowlink(node) = lowlink(node).min(lowlink(neighbor))
      }
    }

    if (lowlink(node) == index(node)) {
      val comp = mutable.Set[N]()
      var neighbor = node
      do {
        neighbor = stack.pop()
        visited(neighbor) = NotVisisted
        comp.add(neighbor)
      } while (node != neighbor)
      components += comp.toSet
    }
    i
  }

  private def getConsideredNodes(withDemand: Boolean): (Set[N], Map[N, Set[(N, E)]]) = if (withDemand) (nodes.toSet, edges.toMap) else {
    val consideredNodes = nodes.filterNot(node => node.toString.startsWith("input$"))
    val consideredEdges = mutable.Map[N, Set[(N, E)]]()

    edges.foreach { edge =>
      val node = edge._1
      val neighbors = edge._2
      val newneighbors = neighbors.filter(n => consideredNodes.contains(n._1))
      if (consideredNodes.contains(node) && newneighbors.nonEmpty) consideredEdges(node) = newneighbors
    }
    (consideredNodes.toSet, consideredEdges.toMap)
  }

  protected def nodeToGraphViz(n: N): String
  protected def edgeGraphVizAttributes(from: N, to: N, info: E): String
  protected def nodeGraphVizAttributes(from: N): String
}

