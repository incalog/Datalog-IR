package org.inca.generator

import analyzedLangs.BinaryExpLang._
import org.inca.lang.core.Constraints.PatternCall
import org.inca.lang.core.Content.{JoinTypeDef, CoreTemporaryVariable}
import org.inca.lang.core.Reference.CoreVariableReference
import org.inca.lang.core.Typ.JoinType
import org.inca.lang.gp.Constraints.{GraphPatternConceptConstraint, PathExpressionConstraint, PatternCompositionConstraint}
import org.inca.lang.gp.Content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.lang.gp.Element.PathElement
import org.inca.meta.MetaElements.{Link, NodeType}

object BinaryExpTest extends App {


  val minusExpType: NodeType = NodeType(classOf[MinusExp])
  val plusExpType: NodeType = NodeType(classOf[PlusExp])

  val plusMinusExpression: JoinType = JoinType(JoinTypeDef("PlusMinusExpression", Seq(minusExpType, plusExpType)))
  val plusMinusExpressionType: NodeType = NodeType(classOf[JoinType])
  val eParam: GraphPatternParameter = GraphPatternParameter("e", Some(plusMinusExpressionType))

  val plusMinusPattern: GraphPattern = GraphPattern(
    "PlusMinus",
    Seq(
      eParam),
    Seq(
      GraphPatternBody(
        Seq(GraphPatternConceptConstraint(
          CoreVariableReference(eParam),
          plusMinusExpression)
        )),
      GraphPatternBody(
        Seq(GraphPatternConceptConstraint(
          CoreVariableReference(eParam),
          plusExpType)
        )),
      GraphPatternBody(
        Seq(GraphPatternConceptConstraint(
          CoreVariableReference(eParam),
          minusExpType)
        ))
    ),
    None)


  val variableDeclarationType = NodeType(classOf[VariableDeclaration])
  val expType = NodeType(classOf[Exp])
  val linkDeclarationType = NodeType(classOf[LinkDeclaration])
  val linkDeclarationLink: Link = linkDeclarationType("name")

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
            CoreVariableReference(varr),
            CoreVariableReference(initializer),
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
  val primitiveDataTypeDeclarationType = NodeType(classOf[PrimitiveDataTypeDeclaration])

  val initializerBoolean = GraphPatternParameter("initializer", Some(primitiveDataTypeDeclarationType))
  val expressionTempVar = CoreTemporaryVariable("expression", None)

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
            CoreVariableReference(varr),
            expressionTempVar,
            PathElement(None, linkDeclarationLink),
            linkDeclarationType
          ),
          PatternCompositionConstraint(
            neg = false,
            PatternCall(
              transitive = false,
              Seq(
                CoreVariableReference(initializerBoolean),
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
