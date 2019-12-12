import org.inca.analyzedLangs.GraphLang.{Edge, Node}
import org.inca.core.Constraints.PatternCall
import org.inca.core.Content.TemporaryVariable
import org.inca.core.Reference.VariableReference
import org.inca.gp.Constraints.{PathExpressionConstraint, PatternCompositionConstraint}
import org.inca.gp.Content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.gp.Element.PathElement
import org.inca.meta.{NodeType, NodeLink}

object GraphLangTest {

  val nodeType = NodeType(classOf[Node])
  val edgeType = NodeType(classOf[Edge])
  val edgeToNodeLink: NodeLink = edgeType("to")
  val srcGraphParam = GraphPatternParameter("src", Some(nodeType))
  val trgGraphParam = GraphPatternParameter("trg", Some(nodeType))
  val intermediate = TemporaryVariable("inter", Some(nodeType))

  val path = GraphPattern(
    "Path",
    Seq(
      srcGraphParam,
      trgGraphParam
    ),
    Seq(
      GraphPatternBody(Seq(
        PathExpressionConstraint(
          VariableReference(srcGraphParam),
          VariableReference(trgGraphParam),
          PathElement(None, edgeToNodeLink),
          edgeType
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
            path
          )
        ),
        PathExpressionConstraint(
          VariableReference(intermediate),
          VariableReference(trgGraphParam),
          PathElement(None, edgeToNodeLink),
          edgeType
        )
      )
      )),
    None
  )
}
