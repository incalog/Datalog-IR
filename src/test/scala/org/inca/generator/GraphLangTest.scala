package org.inca.generator

import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.analyzedLangs.{Edge, Graph, Node}
import org.inca.gen.Pipeline.generateGraphPattern
import org.inca.generator.Util._
import org.inca.generator.generated.DirectEdge
import org.inca.incer.indices.{EnginePool, TFQueryScope}
import org.inca.lang.Core._
import org.inca.lang.Gp._
import org.inca.meta.MetaElements.NodeType
import org.scalatest.funsuite.AnyFunSuite

class GraphLangTest extends AnyFunSuite {

  private val nodeType  = NodeType(classOf[Node])
  private val edgeType  = NodeType(classOf[Edge])
  private val graphType = NodeType(classOf[Graph])

  private val edgeToNodeLink   = edgeType("to")
  private val edgeFromNodeLink = edgeType("from")
  private val nodeParentLink   = nodeType("parent")
  private val graphEdgesLink   = graphType("edges")

  private val srcGraphParam = GraphPatternParameter("src", Some(nodeType))
  private val trgGraphParam = GraphPatternParameter("trg", Some(nodeType))
  private val edgeGraphParam = GraphPatternParameter("edge", Some(edgeType))

  private val intermediate = CoreTemporaryVariable("inter", Some(nodeType))
  private val graph = CoreTemporaryVariable("graph", Some(graphType))
  private val edge = CoreTemporaryVariable("edge", Some(edgeType))

  /**
   * pattern EdgeLoop(edge: Edge) {
   *   edge.from == edge.to
   * }
   */
  private val edgeLoop: GraphPattern = GraphPattern(
    "EdgeLoop",
    Seq(
      edgeGraphParam
    ),
    Seq(

    ),
    None
  );

  /**
   * pattern DirectEdge(src: Node, trg: Node) {
   *   Node.parent(src, graph)
   *   Graph.edges(graph, edge)
   *   Edge.from(edge, src)
   *   Edge.to(edge, trg)
   * }
   */
  private val directEdge: GraphPattern = GraphPattern(
    "DirectEdge",
    Seq(
      srcGraphParam,
      trgGraphParam
    ),
    Seq(
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            CoreVariableReference(srcGraphParam),
            graph,
            ParentPathElement(None, nodeParentLink),
            nodeType
          ),
          PathExpressionConstraint(
            CoreVariableReference(graph),
            edge,
            PathElementImpl(None, graphEdgesLink),
            graphType
          ),
          PathExpressionConstraint(
            CoreVariableReference(edge),
            CoreVariableReference(srcGraphParam),
            PathElementImpl(None, edgeFromNodeLink),
            edgeType
          ),
          PathExpressionConstraint(
            CoreVariableReference(edge),
            CoreVariableReference(trgGraphParam),
            PathElementImpl(None, edgeToNodeLink),
            edgeType
          )
        )
      )
    ),
    None
  )

  /**
   * pattern Path(src: Node, trg: Node) {
   *   find DirectEdge(src, trg)
   * } or {
   *   find DirectEdge(src, intermediate)
   *   find Path(intermediate, trg)
   * }
   */
  private lazy val path: GraphPattern = GraphPattern(
    "Path",
    Seq(
      srcGraphParam,
      trgGraphParam
    ),
    Seq(
      GraphPatternBody(Seq(
        CompositionConstraint(
          neg = false,
          PatternCall(
            transitive = false,
            Seq(
              CoreVariableReference(srcGraphParam),
              CoreVariableReference(trgGraphParam)
            ),
            directEdge
          )
        )
      )),
      GraphPatternBody(Seq(
        CompositionConstraint(
          neg = false,
          PatternCall(
            transitive = false,
            Seq(
              CoreVariableReference(srcGraphParam),
              intermediate
            ),
            directEdge
          )
        ),
        CompositionConstraint(
          neg = false,
          PatternCall(
            transitive = false,
            Seq(
              intermediate,
              CoreVariableReference(trgGraphParam)
            ),
            path
          )
        )
      )
      )),
    None
  )

  /**
   * pattern GreatGrandPa(src : Node, trg : Node) {
   *   Node.parent.parent(src, trg)
   *   Node.parent.parent(src, trg)
   * }
   *
   * Shall be transformed into:
   * pattern GreatGrandPa(src : Node, trg : Node) {
   *   Node.parent(src, temp1)
   *   Node.parent(temp1, temp2)
   *   Node.parent(temp2, temp3)
   *   Node.parent(temp3, trg)
   *
   *   Node.parent(src, temp4)
   *   Node.parent(temp4, temp5)
   *   Node.parent(temp5, temp6)
   *   Node.parent(temp6, trg)
   * }
   */
  private val greatGrandParent: GraphPattern = GraphPattern(
    "GreatGrandParent",
    Seq(
      srcGraphParam,
      trgGraphParam
    ),
    Seq(
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            CoreVariableReference(srcGraphParam),
            CoreVariableReference(trgGraphParam),
            ParentPathElement(
              Some(ParentPathElement(
                Some(ParentPathElement(None, nodeParentLink)),
                nodeParentLink
              )), nodeParentLink),
            nodeType
          ),

          PathExpressionConstraint(
            CoreVariableReference(srcGraphParam),
            CoreVariableReference(trgGraphParam),
            ParentPathElement(
              Some(ParentPathElement(
                Some(ParentPathElement(None, nodeParentLink)),
                nodeParentLink
              )), nodeParentLink),
            nodeType
          )
        )
      )
    ),
    None
  )



  private val node0 = Node("0")
  private val node1 = Node("1")
  private val testGraph = Graph(List(node0, node1), List(Edge("0", "1"), Edge("1", "0")))
  private val testGraph1 = Graph(List(node0, node1), List(Edge("0", "1")))
  private val testGraph2 = Graph(List(node0), List(Edge("0", "0")))


  test("one direction") {
    writeClass(greatGrandParent)

    val scope = new TFQueryScope(testGraph1)
    val matcher = EnginePool.getMatcher(DirectEdge.instance(),
      scope, DifferentialReteBackendFactory.INSTANCE)
    println(matcher.getAllMatches)

    val scope2 = new TFQueryScope(testGraph2)
    val matcher2 = EnginePool.getMatcher(DirectEdge.instance(),
      scope2, DifferentialReteBackendFactory.INSTANCE)
    println(matcher2.getAllMatches)


  }
}
