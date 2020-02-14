package org.inca.generator

import analyzedLangs.{Edge, Graph, Node}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.findbugs.ConfusedInheritance
import org.inca.gen.gp.GeneratorGP.generate
import org.inca.gen.gp.helper.Util._
import org.inca.generator.generated.DirectEdge
import org.inca.incer.indices.{EnginePool, TFQueryScope}
import org.inca.lang.core.Constraints.PatternCall
import org.inca.lang.core.Content.TemporaryVariable
import org.inca.lang.core.Reference.VariableReference
import org.inca.lang.gp.Constraints.{PathExpressionConstraint, PatternCompositionConstraint}
import org.inca.lang.gp.Content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.lang.gp.Element.PathElement
import org.inca.lang.gp.Virtual.ParentPathElement
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

  private val intermediate = TemporaryVariable("inter", Some(nodeType))
  private val graph = TemporaryVariable("graph", Some(graphType))
  private val edge = TemporaryVariable("edge", Some(edgeType))

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
            VariableReference(srcGraphParam),
            graph,
            ParentPathElement(None, nodeParentLink),
            nodeType
          ),
          PathExpressionConstraint(
            VariableReference(graph),
            edge,
            PathElement(None, graphEdgesLink),
            graphType
          ),
          PathExpressionConstraint(
            VariableReference(edge),
            VariableReference(srcGraphParam),
            PathElement(None, edgeFromNodeLink),
            edgeType
          ),
          PathExpressionConstraint(
            VariableReference(edge),
            VariableReference(trgGraphParam),
            PathElement(None, edgeToNodeLink),
            edgeType
          )
        )
      )
    ),
    None
  )

  /**
   * pattern Path(src: Node, trg: Node) {
   * find DirectEdge(src, trg)
   * } or {
   * find DirectEdge(src, intermediate)
   * find Path(intermediate, trg)
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
        PatternCompositionConstraint(
          neg = false,
          PatternCall(
            transitive = false,
            Seq(
              VariableReference(srcGraphParam),
              VariableReference(trgGraphParam)
            ),
            directEdge
          )
        )
      )),
      GraphPatternBody(Seq(
        PatternCompositionConstraint(
          neg = false,
          PatternCall(
            transitive = false,
            Seq(
              VariableReference(srcGraphParam),
              intermediate
            ),
            directEdge
          )
        ),
        PatternCompositionConstraint(
          neg = false,
          PatternCall(
            transitive = false,
            Seq(
              intermediate,
              VariableReference(trgGraphParam)
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
            VariableReference(srcGraphParam),
            VariableReference(trgGraphParam),
            ParentPathElement(
              Some(ParentPathElement(
                Some(ParentPathElement(None, nodeParentLink)),
                nodeParentLink
              )), nodeParentLink),
            nodeType
          ),

          PathExpressionConstraint(
            VariableReference(srcGraphParam),
            VariableReference(trgGraphParam),
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


  test("Generate and write directEdge graph pattern") {
    //  generate(greatGrandParent, "GPLang")
    writeClass(generate(directEdge), directEdge.name)
//    writeClass(generate(path), path.name)
  }

  private val node0 = Node("0")
  private val node1 = Node("1")
  private val testGraph = Graph(Seq(node0, node1), Seq(Edge(node0, node1)))


  test("DirectEdge program matcher") {
    val scope = new TFQueryScope(testGraph)
    val matcher = EnginePool.getMatcher(DirectEdge.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    println(matcher.getAllMatches)
  }
}
