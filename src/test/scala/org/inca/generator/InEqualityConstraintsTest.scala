package org.inca.generator

import analyzedLangs.BinaryExpLang.VariableDeclaration
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.findbugs.{ClassDeclaration, ConfusedInheritance, FieldDeclaration, ProtectedVisibility}
import org.inca.gen.gp.model.PrimitiveConstants.{BooleanConstant, Primitive}
import org.inca.gen.gp.GeneratorGP.generate
import org.inca.gen.gp.helper.Util._
import org.inca.generator.generated.Boolean_PSystemQuery
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

  private val variableDeclarationNT = NodeType(classOf[VariableDeclaration])
  private val metaElementNT = NodeType(classOf[MetaElement])
  private val booleanNT = NodeType(classOf[Primitive])
  private val booleanConstantNT = NodeType(classOf[BooleanConstant])

  private val variableDeclarationNL = variableDeclarationNT("initializer")
  private val booleanConstantNL = booleanConstantNT("value")

  private val expressionGPP = GraphPatternParameter("expression", Some(metaElementNT))
  private val booleanGPP = GraphPatternParameter("value", Some(booleanNT))

  /**
   * pattern Boolean(exp: Expression, value: boolean) {
   *   BooleanConstant.value(expression, value)
   * }
   */
  private val booleanGP: GraphPattern = GraphPattern(
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
  private val varGP = GraphPatternParameter("var", Some(variableDeclarationNT))
  private val initializerGP = GraphPatternParameter("initializer", Some(booleanNT))

  // temporary variables
  private val expressionTV = TemporaryVariable("expression", None)

  /**
   * pattern FalseInitializer(var: VariableDeclaration, initializer: boolean) {
   *   VariableDeclaration.initializer(var, expression)
   *   find Boolean(expression, initializer)
   *   initializer == false
   * }
   */
  private val falseInitializerGP = GraphPattern(
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

  test("Generate and write falseInitializer graph pattern") {
    writeClass(generate(booleanGP), falseInitializerGP.name)
//    print(generate(falseInitializerGP))
  }

  // todo add real example program
  val clazz: ClassDeclaration = ClassDeclaration("Foo", true, List(FieldDeclaration("bar", ProtectedVisibility())))

  test("Use generated code in PSystem") {
    val scope = new TFQueryScope(clazz)
    val matcher = EnginePool.getMatcher(ConfusedInheritance.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    println(matcher.getAllMatches)
  }

  test("PSystem bool pattern") {
    val scope = new TFQueryScope(clazz)
    val matcher = EnginePool.getMatcher(Boolean_PSystemQuery.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    println(matcher.getAllMatches)
  }

}
