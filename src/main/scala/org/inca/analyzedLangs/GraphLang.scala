package org.inca.analyzedLangs

import org.inca.mps.INamedConcept

object GraphLang {

  case class Node(name: String) extends INamedConcept
  case class Edge(from: Node, to: Node)
  case class Graph(nodes: Seq[Node], edges: Seq[Edge])
}
