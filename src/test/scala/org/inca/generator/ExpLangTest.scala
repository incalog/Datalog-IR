package org.inca.generator

import org.inca.analyzedLangs._
import org.eclipse.viatra.query.runtime.api.{IPatternMatch, ViatraQueryMatcher}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.gen.Pipeline.generateGraphPattern
import org.inca.incer.Incrementalizable
import org.inca.incer.indices.{EnginePool, TFQueryScope}
import org.inca.lang.core.Reference.CoreVariableReference
import org.inca.lang.gp.Constraints.GraphPatternConceptConstraint
import org.inca.lang.gp.Content.{GraphPattern, GraphPatternBody, GraphPatternParameter}
import org.inca.meta.MetaElements.NodeType
import org.scalatest.funsuite.AnyFunSuite

class ExpLangTest extends AnyFunSuite {

  private val expType = NodeType(classOf[Expression])
  private val intType = NodeType(classOf[IntegerLit])
  private val longType = NodeType(classOf[LongLit])
  //  private val boolType = NodeType(classOf[BooleanLit])

  private val expGPP = GraphPatternParameter("exp", Some(expType))

  private val numberPattern: GraphPattern = GraphPattern(
    "Number",
    Seq(
      expGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          GraphPatternConceptConstraint(CoreVariableReference(expGPP), longType)
        )
      ),
      GraphPatternBody(
        Seq(
          GraphPatternConceptConstraint(CoreVariableReference(expGPP), intType)
        )
      )
    ),
    None
  )


  val testInput = Add(And(Or(BooleanLit(true), BooleanLit(false)), IntegerLit(5)), IntegerLit(10))

  test("Write `Number` pattern to console") {
    println(generateGraphPattern(numberPattern))
  }
  test("Test `Number` pattern") {
    val scope = new TFQueryScope(testInput)
    val matcher = EnginePool.getMatcher(generated.Number.instance(), scope, DifferentialReteBackendFactory.INSTANCE)
    println(matcher.getAllMatches)
  }
}
