package analyzedLangs

import org.inca.lang.mps.INamedConcept

object GraphLang {

  case class Node(name: String, parent: Node) extends INamedConcept
  case class Edge(from: Node, to: Node)
  case class Graph(nodes: Seq[Node], edges: Seq[Edge])
}
