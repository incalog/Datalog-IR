package org.inca.generator

import analyzedLangs.BinaryExpLang.VariableDeclaration
import org.inca.generator.GraphLangTest.greatGrandParent
import org.inca.generators.gp.GPGenerator
import org.inca.generators.gp.sdk.queryspecification.Primitives.BoolPrimitive
import org.inca.lang.core.Constraints.{EqualityCompareFeature, PatternCall}
import org.inca.lang.core.Content.TemporaryVariable
import org.inca.lang.core.Reference.VariableReference
import org.inca.lang.core.Values.BoolValue
import org.inca.lang.gp.Constraints.{GraphPatternCompareConstraint, PathExpressionConstraint, PatternCompositionConstraint}
import org.inca.lang.gp.Content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.lang.gp.Element.PathElement
import org.inca.meta.MetaElements.{MetaElement, NodeType}

object InEqualityConstraintsTest extends App {

  // todo extract (as primitive constants)
  trait Exp

  case class BooleanConstant(value: Boolean) extends Exp

  // end to-do

  val variableDeclarationNT = NodeType(classOf[VariableDeclaration])
  val metaElementNT = NodeType(classOf[MetaElement])
  val booleanNT = NodeType(classOf[Exp])
  val booleanConstantNT = NodeType(classOf[BooleanConstant])

  val variableDeclarationNL = variableDeclarationNT("initializer")
  val booleanConstantNL = booleanConstantNT("value")

  // graph pattern parameters
  val expressionGPP = GraphPatternParameter("expression", Some(metaElementNT))
  val booleanGPP = GraphPatternParameter("expression", Some(booleanNT))

  val booleanGP: GraphPattern = GraphPattern(
    "Boolean",
    Seq(
      expressionGPP,
      booleanGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(expressionGPP),
            VariableReference(booleanGPP),
            PathElement(None, booleanConstantNL),
            booleanConstantNT
          )
        )
      )
    ),
    None
  )


  // graph pattern parameters
  val varGP = GraphPatternParameter("var", Some(variableDeclarationNT))
  val initializerGP = GraphPatternParameter("initializer", Some(booleanNT))

  // temporary variables
  val expressionTV = TemporaryVariable("expression", None)

  /**
   * pattern FalseInitializer(var: VariableDeclaration, initializer: boolean) {
   *   VariableDeclaration.initializer(var, expression)
   * find Boolean(expression, initializer)
   * initializer == false
   * }
   */
  val falseInitializerGP = GraphPattern(
    "FalseInitializer",
    Seq(
      varGP,
      initializerGP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(varGP),
            expressionTV,
            PathElement(None, variableDeclarationNL),
            variableDeclarationNT
          ),
          PatternCompositionConstraint(
            neg = false,
            PatternCall(
              transitive = false,
              Seq(
                VariableReference(expressionTV),
                VariableReference(initializerGP)
              ),
              booleanGP
            )
          ),
          GraphPatternCompareConstraint(
            EqualityCompareFeature(),
            VariableReference(initializerGP),
            BoolPrimitive(false)
          ),
          GraphPatternCompareConstraint(
            EqualityCompareFeature(),
            BoolPrimitive(true),
            BoolPrimitive(true)
          )
        )
      )
    ),
    None
  )


  val gpgen = new GPGenerator
//  gpgen.generate(booleanGP, "BoolLang")
  gpgen.generate(falseInitializerGP, "BoolLang")
}
