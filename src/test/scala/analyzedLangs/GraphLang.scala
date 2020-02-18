package analyzedLangs

import org.inca.incer.IncrementalIndex

@IncrementalIndex
case class Node(name: String) extends INamedConcept

@IncrementalIndex
case class Edge(from: Node, to: Node)

@IncrementalIndex
case class Graph(nodes: Seq[Node], edges: Seq[Edge])

@IncrementalIndex
case class Forest(graphs: Seq[Graph])


