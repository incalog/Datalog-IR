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
  private val addType = NodeType(classOf[Add])

  private val expGPP = GraphPatternParameter("exp", Some(expType))

  /**
   * pattern Number(exp : Expression) {
   * IntegerLit(exp)
   * } or {
   * LongLit(exp)
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
          ConceptConstraint(VariableReference(expGPP), longType)
        )
      ),
      GraphPatternBody(
        Seq(
          ConceptConstraint(VariableReference(expGPP), intType)
        )
      )
    ),
    None
  )

  /**
   * pattern Boolean(exp : Expression) {
   *   BoolLit(exp)
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
          ConceptConstraint(VariableReference(expGPP), boolType)
        )
      )
    ),
    None
  )

  /**
   * pattern Primitives(exp : Expression) {
   *   find Number(exp)
   * } or {
   *   find Boolean(exp)
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
          CompositionConstraint(neg = false, PatternCall(transitive = false, Seq(VariableReference(expGPP)), numberPattern))
        )
      ),
      GraphPatternBody(
        Seq(
          CompositionConstraint(neg = false, PatternCall(transitive = false, Seq(VariableReference(expGPP)), booleanPattern))
        )
      )
    ),
    None
  )


  private val tempBoolVal = TemporaryVariable("tempVal", Some(boolType))
  private val valueLink = boolType("value")
  /**
   * pattern someTrue(exp : Expression) {
   *   find Boolean(exp)
   *   BooleanLit.value(exp, tempVal)
   *   tempVal == true
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
          CompositionConstraint(neg = false, PatternCall(transitive = false, Seq(VariableReference(expGPP)), booleanPattern)),
          PathExpressionConstraint(
            VariableReference(expGPP),
            tempBoolVal,
            PathElementImpl(None, valueLink), boolType),
          CompareConstraint(EqualityCompareFeature(), tempBoolVal, BooleanLiteral(true))
        )
      )
    ),
    None
  )

  private val addLhsLink = addType("lhs")
  private val addRhsLink = addType("rhs")
  private val tempNumVal1 = TemporaryVariable("tempVal1", Some(expType))
  private val tempNumVal2 = TemporaryVariable("tempVal2", Some(expType))
  private val valueNumericLink = intType("value")

  /**
   * pattern NumericAddition(exp : Expression) {
   *   Add.lhs.value(exp, tempVal)
   * }
   */
  private val numericAddition = GraphPattern(
    "NumericAddition",
    Seq(
      expGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(expGPP),
            tempNumVal1,
            PathElementImpl(
              Some(
                PathElementImpl(
                  None,
                  valueNumericLink)
              ),
              addLhsLink
            ),
            addType)
        )
      )
    ),
    None
  )


  private val testInput =
    Add(
      And(
        Or(BooleanLit(false), BooleanLit(false)),
        IntegerLit(5)),
      LongLit(10L))

  // And(Or(BooleanLit(false), BooleanLit(false)), NumLit(5))
  // remove and_or_lhs
  // deleteNodeLinkInstance(and_lhs, Or.lhs, and_or_lhs) <- remove the edge
//   deleteNodeTypeInstance(BooleanLit, and_or_lhs) <- remove the node being an instance of BooleanLit
  // deleteNodeLinkInstance(and_or_lhs, BooleanLit.value, "false")
  // deleteDataTypeInstance(false)

  private val and_or_lhs: BooleanLit = BooleanLit(false)
  private val and_or_rhs: BooleanLit = BooleanLit(false)
  private val and_lhs: Or = Or(and_or_lhs, and_or_rhs)
  private val and_rhs: IntegerLit = IntegerLit(5)
  private val testInput2 =
      And(
        and_lhs,
        and_rhs
      )

  private val testInputNumericAddition = Add(Add(IntegerLit(5), IntegerLit(7)), Add(LongLit(7), IntegerLit(8)))


  test("Test transformation") {
    writeClass(numericAddition)
    val scope = new TFQueryScope(testInputNumericAddition)
    val matcher = EnginePool.getMatcher(generated.NumericAddition.instance(), scope, DifferentialReteBackendFactory.INSTANCE)

    println("Transform working Matches:")
    // should only the Add() with IntegerLit on its lhs
    println(matcher.getAllMatches)
    assert(!matcher.getAllMatches.isEmpty)
  }

  test("Test constraints, generator") {
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
    assert(truthMatcher.getAllMatches.isEmpty)

    println("\nChanging indices...")
    val indices = scope.getEngineContext.getBaseIndex

//    indices.update(() => {
//      indices.deleteNodeLinkInstance(testInput, NodeType(classOf[Add])("lhs"),
//        And(Or(BooleanLit(true), BooleanLit(false)),IntegerLit(5)))
////      indices.deleteNodeLinkInstance(testInput, NodeType(classOf[Or])("rhs"), BooleanLit(false))
//      indices.deleteDataTypeInstance(And(Or(BooleanLit(true), BooleanLit(false)),IntegerLit(5)))
//      indices.insertNodeLinkInstance(testInput, NodeType(classOf[Add])("lhs"), IntegerLit(2))
//      indices.insertDataTypeInstance(IntegerLit(2))
//    })


    println("Boolean Constraint Matches after update:")
    // should contain the both BooleanLit's with the values true and false
    println(booleanMatcher.getAllMatches)
    assert(!booleanMatcher.getAllMatches.isEmpty)

  }
}
