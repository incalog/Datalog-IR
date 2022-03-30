package inca.debugger.table

import inca.util.Graph
import scala.collection.mutable

class NetworkFlowGraph[N] extends Graph[N, Int] {
  def maximumFlow(source: N, sink: N): Seq[Seq[(N, N)]] =
    edmondsKarp(source, sink)

  def edmondsKarp(source: N, sink: N): Seq[Seq[(N, N)]] = {
    val flow: mutable.Map[(N, N), (Int, Int)] = edges.flatMap { case (from, outgoing) =>
      outgoing.map { case (to, cap) =>
        ((from, to), (0, cap))
      }
    }

    val reverseFlow: mutable.Map[(N, N), Int] = edges.flatMap { case (from, outgoing) =>
      outgoing.map { case (to, _) =>
        ((from, to), 0)
      }
    }

    val selectedPaths = mutable.ListBuffer[Seq[(N, N)]]()
    var path = breathFirstSearch(source, x => x == sink, flow.toMap)

    while (path.nonEmpty) {
      // find min capacity of found path
      val edgesOfPath = getEdgesOfPath(path.getOrElse(Seq()))
      selectedPaths += edgesOfPath
      val minCapacityOfPath = edgesOfPath.map { case (from, to) =>
        edges.get(from) match {
          case Some(out) =>
            val (_, cap) = out.find(_._1 == to).get
            cap
          case _ => throw new IllegalArgumentException(s"edges should contain edge $from -> $to")
        }
      }.min

      // update remainingFlow and reverseFlow for path
      edgesOfPath.foreach { case (from, to) =>
        val (current, cap) = flow((from, to))
        flow((from, to)) = (current + minCapacityOfPath, cap)
        val currentReverse = reverseFlow((from, to))
        reverseFlow((from, to)) = currentReverse - minCapacityOfPath
      }
      // find new path if possible
      path = breathFirstSearch(source, x => x == sink, flow.toMap)
    }
    selectedPaths.toSeq
  }

  private def getEdgesOfPath(path: Seq[N]): Seq[(N, N)] = {
    val edges = mutable.ListBuffer[(N, N)]()
    if (path.nonEmpty) {
      var prevNode = path.head
      path.tail.foreach { node =>
        val edge = (prevNode, node)
        edges += edge
        prevNode = node
      }
      edges.toSeq
    } else {
      Seq()
    }
  }

  def breathFirstSearch(
      start: N,
      f: N => Boolean,
      residual: Map[(N, N), (Int, Int)]
    ): Option[Seq[N]] = {
    val explored = mutable.Set[N]()
    val queue = mutable.Queue[Seq[N]]()
    queue.enqueue(Seq(start))
    explored += start

    while (queue.nonEmpty) {
      val path = queue.dequeue()
      val next = path.last
      if (f(next)) {
        return Some(path)
      } else {
        edges.getOrElse(next, Seq()).foreach { case (to, _) =>
          val (currentFlow, flowCap) = residual((next, to))
          if (!explored.contains(to) && flowCap - currentFlow > 0) {
            queue.enqueue(path :+ to)
          }
        }
      }
    }
    None
  }

  override protected def nodeToGraphViz(n: N): String = s""""${n.toString}""""
  override protected def edgeGraphVizAttributes(from: N, to: N, info: Int): String = ""
  override protected def nodeGraphVizAttributes(from: N): String = ""
}
