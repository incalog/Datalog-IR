package org.inca.generator

import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.analyzedLangs._
import org.inca.generator.Util.writeClass
import org.inca.incer.indices.{EnginePool, TFQueryScope}
import org.inca.lang.Core._
import org.inca.lang.Gp._
import org.inca.meta.MetaElements._
import org.scalatest.funsuite.AnyFunSuite

class CalcLangTest extends AnyFunSuite {

  private val expType = NodeType(classOf[Exp])
  private val intType = NodeType(classOf[Integr])
  private val decType = NodeType(classOf[Decimal])
  private val numType = NodeType(classOf[Number])
  private val mulType = NodeType(classOf[Mul])
  private val addType = NodeType(classOf[Addi])
  private val divType = NodeType(classOf[Div])

  private val expGPP = GraphPatternParameter("exp", Some(expType))
  /**
   * pattern Number(exp : Expression) {
   * IntegerLit(exp)
   * } or {
   * LongLit(exp)
   * }
   */
  private val numberPattern: GraphPattern = GraphPattern(
    "NumberCalcLang",
    Seq(
      expGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          ConceptConstraint(VariableReference(expGPP), decType)
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

  private val tempNumVar = TemporaryVariable("divisor", Some(numType))
  private val tempIntVar = TemporaryVariable("divisor", Some(intType))
  private val tempDecVar = TemporaryVariable("divisor", Some(decType))
  private val numValueLink = numType("value")
  private val intValueLink = intType("value")
  private val decValueLink = decType("value")
  private val divRhsLink = divType("rhs")

  /**
   * pattern DivideByZero(exp: Exp) { // integer
   *   Div(exp)
   *   Div.rhs.value(exp, divisor)
   *   divisor == 0
   * } or { // decimal
   *   Div(exp)
   *   Div.rhs.value(exp, divisor)
   *   divisor == 0.0
   * }
   */
  private val divideByZeroPattern: GraphPattern = GraphPattern(
    "DivideByZeroCalcLang",
    Seq(
      expGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          ConceptConstraint(VariableReference(expGPP), divType),
          PathExpressionConstraint(
            VariableReference(expGPP),
            tempIntVar,
            PathElementImpl(
              Some(
                PathElementImpl(
                  None,
                  intValueLink)
              ),
              divRhsLink
            ),
            divType),
//          CompositionConstraint(neg = false, PatternCall(transitive = false, Seq(VariableReference(expGPP)), numberPattern)),
          CompareConstraint(EqualityCompareFeature(), tempIntVar, IntegerLiteral(0))
        )
      ),
      GraphPatternBody(
        Seq(
          ConceptConstraint(VariableReference(expGPP), divType),
          PathExpressionConstraint(
            VariableReference(expGPP),
            tempDecVar,
            PathElementImpl(
              Some(
                PathElementImpl(
                  None,
                  decValueLink)
              ),
              divRhsLink
            ),
            divType),
//          CompositionConstraint(neg = false, PatternCall(transitive = false, Seq(VariableReference(expGPP)), numberPattern)),
          CompareConstraint(EqualityCompareFeature(), tempDecVar, DecimalLiteral(0.0))
        )
      ),
    ),
    None
  )

  private val mulRhsLink = mulType("rhs")
  private val mulLhsLink = mulType("lhs")

  /**
   * pattern IgnoreMultiplication(exp: Exp) {
   *   Mul(exp)
   *   Mul.lhs.value(exp, value)
   *   value == 0
   * } or {
   *   Mul(exp)
   *   Mul.rhs.value(exp, value)
   *   value == 0
   * }
   */
  private val ignoreMultPattern: GraphPattern = GraphPattern(
    "IgnoreMultiplicationCalcLang",
    Seq(
      expGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          ConceptConstraint(VariableReference(expGPP), mulType),
          PathExpressionConstraint(
            VariableReference(expGPP),
            tempNumVar,
            PathElementImpl(
              Some(
                PathElementImpl(
                  None,
                  numValueLink)
              ),
              mulLhsLink
            ),
            mulType),
          CompareConstraint(EqualityCompareFeature(), tempNumVar, DecimalLiteral(0.0))
        )
      ),
      GraphPatternBody(
        Seq(
          ConceptConstraint(VariableReference(expGPP), mulType),
          PathExpressionConstraint(
            VariableReference(expGPP),
            tempNumVar,
            PathElementImpl(
              Some(
                PathElementImpl(
                  None,
                  numValueLink)
              ),
              mulRhsLink
            ),
            mulType),
          CompareConstraint(EqualityCompareFeature(), tempNumVar, DecimalLiteral(0.0))
        )
      )

    ),
    None
  )

  // number-pattern test inputs
  private val numTestInput_1 = Mul( Addi(Integr(5), Decimal(1.3)), Div(Decimal(20.0), Integr(20)))


  test("Test number pattern") {
    writeClass(numberPattern)

    // test number pattern
    val scope_numTestInput_1 = new TFQueryScope(numTestInput_1)
    val numTestInput_1_Matcher = EnginePool.getMatcher(generated.NumberCalcLang.instance(), scope_numTestInput_1, DifferentialReteBackendFactory.INSTANCE)


    println("numTestInput_1 matches:")
    println(numTestInput_1_Matcher.getAllMatches)
    assert(!numTestInput_1_Matcher.getAllMatches.isEmpty)
  }

  // div-by-zero-pattern test inputs

  private val divByZeroTestInput_1 = Div(Integr(1), Integr(0))
  private val divByZeroTestInput_2 = Div(Integr(1), Decimal(0.0))
  private val divByZeroTestInput_3 = Div(Integr(0), Integr(1))
  private val divByZeroTestInput_4 = Div(Integr(0), Decimal(1))
  private val divByZeroTestInput_5 = Mul( Addi(Integr(0), Decimal(0.0)), Decimal(0.0))

  test("Test divide-by-zero pattern") {
    writeClass(divideByZeroPattern)


    val scope_divTestInput_1 = new TFQueryScope(divByZeroTestInput_1)
    val scope_divTestInput_2 = new TFQueryScope(divByZeroTestInput_2)
    val scope_divTestInput_3 = new TFQueryScope(divByZeroTestInput_3)
    val scope_divTestInput_4 = new TFQueryScope(divByZeroTestInput_4)
    val scope_divTestInput_5 = new TFQueryScope(divByZeroTestInput_5)
    val divTestInput_1_Matcher = EnginePool.getMatcher(generated.DivideByZeroCalcLang.instance(), scope_divTestInput_1, DifferentialReteBackendFactory.INSTANCE)
    val divTestInput_2_Matcher = EnginePool.getMatcher(generated.DivideByZeroCalcLang.instance(), scope_divTestInput_2, DifferentialReteBackendFactory.INSTANCE)
    val divTestInput_3_Matcher = EnginePool.getMatcher(generated.DivideByZeroCalcLang.instance(), scope_divTestInput_3, DifferentialReteBackendFactory.INSTANCE)
    val divTestInput_4_Matcher = EnginePool.getMatcher(generated.DivideByZeroCalcLang.instance(), scope_divTestInput_4, DifferentialReteBackendFactory.INSTANCE)
    val divTestInput_5_Matcher = EnginePool.getMatcher(generated.DivideByZeroCalcLang.instance(), scope_divTestInput_5, DifferentialReteBackendFactory.INSTANCE)

    println("divTestInput_1 matches:")
    println(divTestInput_1_Matcher.getAllMatches)
//    assert(!divTestInput_1_Matcher.getAllMatches.isEmpty)
    println("divTestInput_2 matches:")
    println(divTestInput_2_Matcher.getAllMatches)
//    assert(!divTestInput_2_Matcher.getAllMatches.isEmpty)


    println("divTestInput_3 matches:")
    println(divTestInput_3_Matcher.getAllMatches)
//    assert(divTestInput_3_Matcher.getAllMatches.isEmpty)
    println("divTestInput_4 matches:")
    println(divTestInput_4_Matcher.getAllMatches)
//    assert(divTestInput_4_Matcher.getAllMatches.isEmpty)
    println("divTestInput_5 matches:")
    println(divTestInput_5_Matcher.getAllMatches)
//    assert(divTestInput_5_Matcher.getAllMatches.isEmpty)
  }

  test("Test ignore-multiplication pattern") {
    writeClass(ignoreMultPattern)
  }
}
