package org.inca.analyzedLangs

import org.inca.mps.INamedConcept

object GraphLang {

  case class Graph(nodes: List[Node], edges: List[Edge])
  case class Node(name: s
                 ) extends INamedConcept
  case class Edge(from: Node, to: Node)
}
