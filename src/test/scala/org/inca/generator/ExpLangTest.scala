package org.inca.generator

import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.analyzedLangs._
import org.inca.gen.Pipeline.generateGraphPattern
import org.inca.incer.indices.{EnginePool, TFQueryScope}
import org.inca.lang.Core.{CoreVariableReference, PatternCall}
import org.inca.lang.Gp._
import org.inca.meta.MetaElements.NodeType
import org.scalatest.funsuite.AnyFunSuite
import Util._

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

  private val testInput = Add(And(Or(BooleanLit(true), BooleanLit(false)), IntegerLit(5)), LongLit(10))

  /**
   * Do this first, if you have no pattern file, else you can skip
   */
  test("Generate patterns") {
    writeClass(numberPattern)
    writeClass(booleanPattern)
    writeClass(primitivesPattern)
  }

  test("Test concept and composition constraint") {
    // updates generated files
    writeClass(numberPattern)
    writeClass(booleanPattern)
    writeClass(primitivesPattern)

    val scope = new TFQueryScope(testInput)

    val numberMatcher = EnginePool.getMatcher(generated.Number.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    val primitivesMatcher = EnginePool.getMatcher(generated.Primitives.instance(), scope, DifferentialReteBackendFactory.INSTANCE)

    println("Concept Constraint Matches:")
    // should contain both, the LongLit with value 10 and the IntegerLit with value 5
    println(numberMatcher.getAllMatches)
    assert(!numberMatcher.getAllMatches.isEmpty)

    println("Composition Constraint Matches:")
    // should contain the previous values and
    // the two BooleanLit's with false and true als values
    println(primitivesMatcher.getAllMatches)
    assert(!primitivesMatcher.getAllMatches.isEmpty)

  }
}
