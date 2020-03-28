package org.inca.generator

import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.analyzedLangs._
import org.inca.gen.Pipeline.generateGraphPattern
import org.inca.incer.indices.{EnginePool, TFQueryScope}
import org.inca.lang.Core.CoreVariableReference
import org.inca.lang.Gp._
import org.inca.meta.MetaElements.NodeType
import org.scalatest.funsuite.AnyFunSuite
import Util._

class ExpLangTest extends AnyFunSuite {

  private val expType = NodeType(classOf[Expression])
  private val intType = NodeType(classOf[IntegerLit])
  private val longType = NodeType(classOf[LongLit])
  //  private val boolType = NodeType(classOf[BooleanLit])

  private val expGPP = GraphPatternParameter("exp", Some(expType))

  /**
   * pattern Number(exp : Expression) {
   *   IntegerLit(exp)
   * } or {
   *   LongLit(exp)
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


  private val testInput = Add(And(Or(BooleanLit(true), BooleanLit(false)), IntegerLit(5)), LongLit(10))

  test("Test `Number` pattern") {
    writeClass(numberPattern)

    val scope = new TFQueryScope(testInput)
    val matcher = EnginePool.getMatcher(generated.Number.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    println(matcher.getAllMatches)
  }
}
