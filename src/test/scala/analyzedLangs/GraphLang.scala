package analyzedLangs

import org.inca.incer.IncrementalIndex
import org.inca.lang.mps.INamedConcept

@IncrementalIndex
case class Node(name: String) extends INamedConcept

@IncrementalIndex
case class Edge(from: Node, to: Node)

@IncrementalIndex
case class Graph(nodes: Seq[Node], edges: Seq[Edge])

@IncrementalIndex
case class Forest(graphs: Seq[Graph])


