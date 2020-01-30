import analyzedLangs.GraphLang.{Edge, Graph, Node}
import org.inca.lang.core.Constraints.{EqualityCompareFeature, PatternCall, Something}
import org.inca.lang.core.Content.TemporaryVariable
import org.inca.lang.core.Reference.VariableReference
import org.inca.generators.gp.GPGenerator
import org.inca.lang.core.Typp.ConceptReferenceType
import org.inca.lang.gp.Constraints.{GraphPatternCompareConstraint, PathExpressionConstraint, PatternCompositionConstraint}
import org.inca.lang.gp.Content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.lang.gp.Element.PathElement
import org.inca.lang.gp.Virtual.ParentPathElement
import org.inca.lang.meta.{NodeLink, NodeType}

object GraphLangTest extends App {

  val nodeType = NodeType(classOf[Node])
  val edgeType = NodeType(classOf[Edge])
  val graphType = NodeType(classOf[Graph])

  val edgeToNodeLink: NodeLink = edgeType("to")
  val edgeFromNodeLink: NodeLink = edgeType("from")
  val nodeParentLink: NodeLink = nodeType("parent")
  val graphEdgesLink: NodeLink = graphType("edges")

  val srcGraphParam = GraphPatternParameter("src", Some(nodeType))
  val trgGraphParam = GraphPatternParameter("trg", Some(nodeType))
  // temp vars
  val intermediate = TemporaryVariable("inter", Some(nodeType))
  val graph = TemporaryVariable("graph", Some(graphType))
  val edge = TemporaryVariable("edge", Some(edgeType))

  /**
   * pattern DirectEdge(src: Node, trg: Node) {
   *   Node.parent(src, graph)
   *   Graph.edges(graph, edge)
   *   Edge.from(edge, src)
   *   Edge.to(edge, trg)
   * }
   */
  val directEdge: GraphPattern = GraphPattern(
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
      ),
      GraphPatternBody(
        Seq(
          GraphPatternCompareConstraint(
            EqualityCompareFeature(),
            Something("left"),
            Something("right")
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
  val path: GraphPattern = GraphPattern(
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
        // todo `neg` should be call
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
   *   Node.parent.parent.parent(src, trg)
   * }
   */
  val greatGrandPa: GraphPattern = GraphPattern(
    "GreatGrandPa",
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
          )
        )
      )
    ),
    None
  )

  val parent_1 = TemporaryVariable("parent_3_0", Some(edgeType))
  val parent_2 = TemporaryVariable("parent_3_1", Some(edgeType))

  /**
   * This is an example which shall show the correct transformation of the previous.
   *
   * pattern GreatGrandPa(src : Node, trg : Node) {
   *   Node.parent(src, temp1)
   *   Node.parent(temp1, temp2)
   *   Node.parent(temp2, temp3)
   *   Node.parent(temp3, trg)
   * }
   */
  val greatGrandPa2: GraphPattern = GraphPattern(
    "GreatGrandPa",
    Seq(
      srcGraphParam,
      trgGraphParam
    ),
    Seq(
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(srcGraphParam),
            parent_1,
            ParentPathElement(None, nodeParentLink),
            nodeType
          ),
          PathExpressionConstraint(
            VariableReference(parent_1),
            parent_2,
            ParentPathElement(None, nodeParentLink),
            nodeType
          ),
          PathExpressionConstraint(
            VariableReference(parent_2),
            VariableReference(trgGraphParam),
            ParentPathElement(None, nodeParentLink),
            nodeType
          ),
        )
      )
    ),
    None
  )

  val gpgen = new GPGenerator
  gpgen.generate(greatGrandPa, "GPLang")
//  gpgen.generate(directEdge, "GPLang")
//  gpgen.generate(path, "GPLang")
}
