package org.inca.generator

import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.analyzedLangs._
import org.inca.generator.Util._
import org.inca.incer.indices.{EnginePool, TFQueryScope}
import org.inca.lang.Core._
import org.inca.lang.Gp._
import org.inca.meta.MetaElements.NodeType
import org.scalatest.funsuite.AnyFunSuite

class ExpLangTest extends AnyFunSuite {

  private val expType = NodeType(classOf[Expression])
  private val intType = NodeType(classOf[IntegerLit])
  private val longType = NodeType(classOf[LongLit])
  private val boolType = NodeType(classOf[BooleanLit])

  private val expGPP = GraphPatternParameter("exp", Some(expType))

  /**
   * pattern Number(exp : Expression) {
   *  IntegerLit(exp)
   * } or {
   *  LongLit(exp)
   * }
   */
  private val numberPattern: GraphPattern = GraphPattern(
    "Number",
    Seq(
      expGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          ConceptConstraint(CoreVariableReference(expGPP), longType)
        )
      ),
      GraphPatternBody(
        Seq(
          ConceptConstraint(CoreVariableReference(expGPP), intType)
        )
      )
    ),
    None
  )

  /**
   * pattern Boolean(exp : Expression) {
   *  IntegerLit(exp)
   * } or {
   *  LongLit(exp)
   * }
   */
  private val booleanPattern: GraphPattern = GraphPattern(
    "Boolean",
    Seq(
      expGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          ConceptConstraint(CoreVariableReference(expGPP), boolType)
        )
      )
    ),
    None
  )

  /**
   * pattern Primitives(exp : Expression) {
   *  find Number(exp)
   * } or {
   *  find Boolean(exp)
   * } // or String or ...
   */
  private val primitivesPattern: GraphPattern = GraphPattern(
    "Primitives",
    Seq(
      expGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          CompositionConstraint(neg = false, PatternCall(transitive = false, Seq(CoreVariableReference(expGPP)), numberPattern))
        )
      ),
      GraphPatternBody(
        Seq(
          CompositionConstraint(neg = false, PatternCall(transitive = false, Seq(CoreVariableReference(expGPP)), booleanPattern))
        )
      )
    ),
    None
  )

  private val tempBoolVal = CoreTemporaryVariable("value", Some(boolType))
  private val valueLink   = boolType("value")


  /**
   * pattern someTrue(exp : Expression) {
   *   find Boolean(exp)
   *   exp == BooleanLit(true)
   * }
   */
  private val someTruth: GraphPattern = GraphPattern(
    "Truth",
    Seq(
      expGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          CompositionConstraint(neg = false, PatternCall(transitive = false, Seq(CoreVariableReference(expGPP)), booleanPattern)),
          PathExpressionConstraint(CoreVariableReference(expGPP), tempBoolVal, PathElementImpl(None, valueLink), boolType),
          CompareConstraint(InequalityCompareFeature(), tempBoolVal, BooleanLiteral(true))
        )
      )
    ),
    None
  )


  private val testInput = Add(And(Or(BooleanLit(true), BooleanLit(false)), IntegerLit(5)), LongLit(10L))


  test("Test concept and composition constraint") {
    // updates generated files
    writeClass(numberPattern)
    writeClass(booleanPattern)
    writeClass(primitivesPattern)
    writeClass(someTruth)

    val scope = new TFQueryScope(testInput)

    val numberMatcher = EnginePool.getMatcher(generated.Number.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    val primitivesMatcher = EnginePool.getMatcher(generated.Primitives.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    val booleanMatcher = EnginePool.getMatcher(generated.Boolean.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    val truthMatcher = EnginePool.getMatcher(generated.Truth.instance(), scope, DifferentialReteBackendFactory.INSTANCE)

    println("Concept Constraint Matches:")
    // should contain both, the LongLit with value 10 and the IntegerLit with value 5
    println(numberMatcher.getAllMatches)
    assert(!numberMatcher.getAllMatches.isEmpty)

    println("Composition Constraint Matches:")
    // should contain the previous values and
    // the two BooleanLit's with false and true als values
    println(primitivesMatcher.getAllMatches)
    assert(!primitivesMatcher.getAllMatches.isEmpty)

    println("Boolean Constraint Matches:")
    // should contain the both BooleanLit's with the values true and false
    println(booleanMatcher.getAllMatches)
    assert(!booleanMatcher.getAllMatches.isEmpty)

    println("Compare Constraint Matches:")
    // should contain the one BooleanLit with the value true
    println(truthMatcher.getAllMatches)
    assert(!truthMatcher.getAllMatches.isEmpty)

  }
}
