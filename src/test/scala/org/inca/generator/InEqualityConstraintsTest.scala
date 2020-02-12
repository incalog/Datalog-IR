package org.inca.generator

import analyzedLangs.BinaryExpLang.VariableDeclaration
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.findbugs.{ClassDeclaration, ConfusedInheritance, FieldDeclaration, ProtectedVisibility}
import org.inca.gen.gp.GPGenerator
import org.inca.gen.gp.sdk.queryspecification.PrimitiveConstants.{BooleanConstant, Primitive}
import org.inca.incer.indices.{EnginePool, TFQueryScope}
import org.inca.lang.core.Constraints.{EqualityCompareFeature, PatternCall}
import org.inca.lang.core.Content.TemporaryVariable
import org.inca.lang.core.Reference.VariableReference
import org.inca.lang.gp.Constraints.{GraphPatternCompareConstraint, PathExpressionConstraint, PatternCompositionConstraint}
import org.inca.lang.gp.Content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.lang.gp.Element.PathElement
import org.inca.meta.MetaElements.{MetaElement, NodeType}
import org.scalatest.funsuite.AnyFunSuite

class InEqualityConstraintsTest extends AnyFunSuite {

  val variableDeclarationNT = NodeType(classOf[VariableDeclaration])
  val metaElementNT = NodeType(classOf[MetaElement])
  val booleanNT = NodeType(classOf[Primitive])
  val booleanConstantNT = NodeType(classOf[BooleanConstant])

  val variableDeclarationNL = variableDeclarationNT("initializer")
  val booleanConstantNL = booleanConstantNT("value")

  // graph pattern parameters
  val expressionGPP = GraphPatternParameter("expression", Some(metaElementNT))
  val booleanGPP = GraphPatternParameter("value", Some(booleanNT))

  val booleanGP: GraphPattern = GraphPattern(
    "Boolean_PSystemQuery",
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
    "FalseInitializer_PSystemQuery",
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
            BooleanConstant(false)
          ),
          GraphPatternCompareConstraint(
            EqualityCompareFeature(),
            BooleanConstant(true),
            BooleanConstant(true)
          )
        )
      )
    ),
    None
  )

  test("Generate Scala Code") {
    val gpgen = new GPGenerator
    gpgen.generate(booleanGP)
    gpgen.generate(falseInitializerGP)
  }

  val clazz = ClassDeclaration("Foo", true, List(FieldDeclaration("bar", ProtectedVisibility())))

  test("Use generated code in PSystem") {
    val scope = new TFQueryScope(clazz)
    val matcher = EnginePool.getMatcher(ConfusedInheritance.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    println(matcher.getAllMatches)
  }

  test("PSystem bool pattern") {
    val scope = new TFQueryScope(clazz)
    val matcher = EnginePool.getMatcher(Boolean_BoolLangQuerySpecification.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    println(matcher.getAllMatches)
  }
}
