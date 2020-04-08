package org.inca.analyzedLangs

import org.inca.incer.IncrementalIndex
import org.inca.lang.Core.Named

@IncrementalIndex
case class Node(name: String) extends Named

@IncrementalIndex
case class Edge(from: String, to: String)

@IncrementalIndex
case class Graph(nodes: List[Node], edges: List[Edge])

@IncrementalIndex
case class Forest(graphs: List[Graph])


