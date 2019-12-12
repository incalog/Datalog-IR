import org.inca.analyzedLangs.BinaryExpLang._
import org.inca.core.Constraints.PatternCall
import org.inca.core.Content.{JoinTypeDef, TemporaryVariable}
import org.inca.core.Reference.VariableReference
import org.inca.core.Typp.JoinType
import org.inca.gp.Constraints.{GraphPatternConceptConstraint, PathExpressionConstraint, PatternCompositionConstraint}
import org.inca.gp.Content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.gp.Element.PathElement
import org.inca.meta.{NodeLink, NodeType}

class BinaryExpTest {

  val minusExpType = NodeType(classOf[MinusExp])
  val plusExpType = NodeType(classOf[PlusExp])

  val plusMinusExpression = JoinType(JoinTypeDef("PlusMinusExpression", Seq(minusExpType, plusExpType)))
  // todo is `.type` correct?
  val plusMinusExpressionType = NodeType(classOf[plusMinusExpression.type])
  val eParam = GraphPatternParameter("e", Some(plusMinusExpressionType))

  val plusMinusPattern = GraphPattern(
    "PlusMinus",
    Seq(
      eParam),
    Seq(
      GraphPatternBody(
        Seq(GraphPatternConceptConstraint(
          VariableReference(eParam),
          plusMinusExpression)
        )),
      GraphPatternBody(
        Seq(GraphPatternConceptConstraint(
          VariableReference(eParam),
          plusExpType)
        )),
      GraphPatternBody(
        Seq(GraphPatternConceptConstraint(
          VariableReference(eParam),
          minusExpType)
        ))
    ),
    None)


  val variableDeclarationType = NodeType(classOf[VariableDeclaration])
  val expType = NodeType(classOf[Exp])
  // todo richtig interpretiert?
  val linkDeclarationType = NodeType(classOf[LinkDeclaration])
  val linkDeclarationLink: NodeLink = linkDeclarationType("name")

  val varr = GraphPatternParameter("var", Some(variableDeclarationType))
  val initializer = GraphPatternParameter("initializer", Some(expType))

  val variableInitializer = GraphPattern(
    "VariableInitializer",
    Seq(
      varr,
      initializer
    ),
    Seq(
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(varr),
            VariableReference(initializer),
            PathElement(None, linkDeclarationLink),
            linkDeclarationType
          )
        )
      )
    ),
    None
  )

  val expressionType = NodeType(classOf[Exp])
  val expression = GraphPatternParameter("expression", Some(expressionType))

  val booleanType = NodeType(classOf[Boolean])
  val value = GraphPatternParameter("value", Some(booleanType))

  val boolean = GraphPattern(
    "Boolean",
    Seq(
      expression,
      value
    ),
    Seq(

    ),
    None
  )

  val initializerBoolean = GraphPatternParameter("initializer", Some(PrimitiveDataTypeDeclaration("boolean")))
  val expressionTempVar = TemporaryVariable("expression", None)

  val variableInitializerWithFalseInitializer = GraphPattern(
    "VariableInitializerWithFalseInitializer",
    Seq(
      varr,
      initializerBoolean
    ),
    Seq(
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(varr),
            expressionTempVar,
            PathElement(None, linkDeclarationLink),
            linkDeclarationType
          ),
          PatternCompositionConstraint(
            neg = false,
            PatternCall(
              transitive = false,
              Seq(
                VariableReference(initializerBoolean),
                expressionTempVar
              ),
              boolean
            )
          )
        )
      )
    ),
    None
  )
}
