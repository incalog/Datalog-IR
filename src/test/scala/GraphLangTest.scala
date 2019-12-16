import analyzedLangs.GraphLang.{Edge, Node}
import org.inca.core.Constraints.PatternCall
import org.inca.core.Content.TemporaryVariable
import org.inca.core.Reference.VariableReference
import org.inca.generators.gp.GPGenerator
import org.inca.gp.Constraints.{PathExpressionConstraint, PatternCompositionConstraint}
import org.inca.gp.Content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.gp.Element.PathElement
import org.inca.meta.{NodeLink, NodeType}

object GraphLangTest extends App{

  val nodeType = NodeType(classOf[Node])
  val edgeType = NodeType(classOf[Edge])
  val edgeToNodeLink: NodeLink = edgeType("to")
  val srcGraphParam = GraphPatternParameter("src", Some(nodeType))
  val trgGraphParam = GraphPatternParameter("trg", Some(nodeType))
  val intermediate = TemporaryVariable("inter", Some(nodeType))

  val path: GraphPattern = GraphPattern(
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
            path: GraphPattern
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

  val gpgen = new GPGenerator
  gpgen.generate(path, "GPLang")
}
