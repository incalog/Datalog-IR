import analyzedLangs.GraphLang.{Edge, Graph, Node}
import org.inca.lang.core.Constraints.PatternCall
import org.inca.lang.core.Content.TemporaryVariable
import org.inca.lang.core.Reference.VariableReference
import org.inca.generators.gp.GPGenerator
import org.inca.lang.core.Typp.ConceptReferenceType
import org.inca.lang.gp.Constraints.{PathExpressionConstraint, PatternCompositionConstraint}
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
  val graph = TemporaryVariable("graph", Some(nodeType))
  val edge = TemporaryVariable("graph", Some(nodeType))

  /**
   * pattern DirectEdge(src: Node, trg: Node) {
   *   Node.parent(src, graph)
   *   Graph.edges(graph, edge)
   *   Edge.from(edge, src)
   *   Edge.to(edge, trg)
   * }
   */
  val directEdge: GraphPattern = GraphPattern(
    "Path",
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

  val gpgen = new GPGenerator
  gpgen.generate(directEdge, "GPLang")
  gpgen.generate(path, "GPLang")
}
