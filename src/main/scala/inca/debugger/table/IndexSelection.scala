package inca.debugger.table

import scala.collection.mutable

// implementing algorithm from paper "Automatic Index Selection for Large-Scale Datalog Computation"
object IndexSelection {

  def minChainCover(searches: Set[Search]): Set[SearchChain] = {
    val edges = searches.flatMap { s =>
      val sPrime = searches.filter(s.subsetOf)
      sPrime.map(s -> _)
    }
    val graph = BipartiteGraph(searches, searches, edges.toSeq)
    val maxMatching = graph.maxMatching
    val chainCovers: mutable.HashSet[SearchChain] = mutable.HashSet()
    for (
      search <- searches
      if maxMatching.forall(_._2 != search)
    ) {
      val maxPath = BipartiteGraph.maxPathFrom[Search](search, maxMatching)
      // there is no path in the max matching starting with search, hence it is a singleton search chain
      if (maxPath.isEmpty) {
        chainCovers += SearchChain(Seq(search))
      } else {
        val chainCover = maxPath.flatMap { case (f, t) => Seq(f, t) }.distinct
        chainCovers += SearchChain(chainCover)
      }
    }
    chainCovers.toSet
  }

  def minIndex(searches: Set[Search]): SearchChain = {
    val minCV = minChainCover(searches)
    val indices =
      for (cv <- minCV) yield {
        val head = cv.searches.head.index
        if (cv.searches.size > 1) {
          val tail = cv.searches.zipWithIndex.tail.map { case (search, idx) =>
            val prev = cv.searches(idx - 1)
            val diff = search.diff(prev)
            if (diff.size != 1) {
              throw new IllegalArgumentException("Size of diff is not 1 in minIndex")
            } else
              diff.index.head
          }
          Search(head ++ tail)
        } else {
          Search(head)
        }
      }
    SearchChain(indices.toSeq)
  }
}

// single search, e.g. x < y
case class Search(index: Seq[String]) {
  def contains(x: String): Boolean = index.contains(x)

  def size: Int = index.size

  def subsetOf(other: Search): Boolean =
    size < other.size && index.forall(other.contains)

  def diff(other: Search): Search =
    Search(index.diff(other.index))
}
object Search {
  def from(args: String*): Search = Search(args)
}

// multiple searches
// e.g. x < y, x < y < z
case class SearchChain(searches: Seq[Search]) {
  def isValid: Boolean = {
    searches.tail.zipWithIndex.forall { case (search, idx) =>
      val prev = searches(idx - 1)
      prev.subsetOf(search)
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case SearchChain(otherSearches) =>
      searches.size == otherSearches.size && searches.forall(otherSearches.contains)
    case _ => false
  }
}

// A graph that have two sets of nodes. Edges can only exist between node x in left and y in right.
// Hence, there are no cycles in this type of graph
case class BipartiteGraph[V](left: Set[V], right: Set[V], edges: Seq[(V, V)]) {

  trait Node
  case object StartNode extends Node
  case object EndNode extends Node
  class VNode(val v: V) extends Node

  lazy val maxMatching: Seq[(V, V)] = {
    val netFlowGraph = constructNetFlowGraph
    val maxPaths = netFlowGraph.maximumFlow(StartNode, EndNode)
    maxPaths.flatMap { path =>
      path.collectFirst { case (from: VNode, to: VNode) =>
        (from.v, to.v)
      }
    }
  }

  private def constructNetFlowGraph: NetworkFlowGraph[Node] = {
    val networkFlowGraph = new NetworkFlowGraph[Node]
    networkFlowGraph.addNode(StartNode)
    networkFlowGraph.addNode(EndNode)
    val leftMap = left.map { vertex =>
      vertex -> new VNode(vertex)
    }.toMap
    val rightMap = right.map { vertex =>
      vertex -> new VNode(vertex)
    }.toMap
    edges.foreach { case (from, to) =>
      networkFlowGraph.addEdge(leftMap(from), rightMap(to), 1)
    }
    left.foreach { uVertex =>
      networkFlowGraph.addEdge(StartNode, leftMap(uVertex), 1)
    }
    right.foreach { vVertex =>
      networkFlowGraph.addEdge(rightMap(vVertex), EndNode, 1)
    }
    networkFlowGraph
  }
}
object BipartiteGraph {
  def apply(searches: Seq[Search]): BipartiteGraph[Search] = {
    val edges = searches.flatMap { search =>
      val subsets = searches.filter(search.subsetOf)
      subsets.map { search -> _ }
    }
    BipartiteGraph(searches.toSet, searches.toSet, edges)
  }

  def maxPathFrom[V](from: V, edges: Seq[(V, V)]): Seq[(V, V)] = {
    def paths(from: V): Seq[Seq[(V, V)]] = {
      val out = outgoing(from)
      val outEdges = out.map(from -> _)
      val newPaths = for {
        to <- out
        newPath <- paths(to)
      } yield {
        (from, to) +: newPath
      }
      outEdges +: newPaths
    }

    def outgoing(from: V): Seq[V] = edges.filter(_._1 == from).map(_._2)

    val potentialPaths = paths(from).map { path => path -> path.size }
    val (maxPath, _) = potentialPaths.maxBy(_._2)
    maxPath
  }
}
