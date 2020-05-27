package org.inca.generator

import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.analyzedLangs.calcLang._
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
  private val mulType = NodeType(classOf[Mul])
  private val divType = NodeType(classOf[Div])

  private val expGPP = GraphPatternParameter("exp", Some(expType))

  private val intValueLink = intType("value")
  private val decValueLink = decType("value")
  private val divRhsLink = divType("rhs")

  private val tempRhsVar = TemporaryVariable("rhs", Some(expType))
  private val divGPP = GraphPatternParameter("div", Some(divType))


  private val intTempVar = TemporaryVariable("int", Some(intType))
  private val decTempVar = TemporaryVariable("dec", Some(decType))
  /**
   * pattern IsZero(exp: Exp) {
   *   Integer.value(exp, int)
   *   int == 0
   * } or {
   *   Decimal.value(exp, dec)
   *   dec == 0.0
   * } or {
   *   exp == 0
   * } or {
   *   exp == 0.0
   */
  private val isZeroPattern = GraphPattern(
    "IsZeroCalcLang",
    Seq(
      expGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(expGPP),
            intTempVar,
            PathElementImpl(
              None,
              intValueLink
            ),
            intType),
          CompareConstraint(EqualityCompareFeature(), intTempVar, IntegerLiteral(0))
        )
      ),
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(expGPP),
            decTempVar,
            PathElementImpl(
              None,
              decValueLink
            ),
            decType),
          CompareConstraint(EqualityCompareFeature(), decTempVar, DecimalLiteral(0.0))
        )
      )
    ),
    None
  )

  /**
   * pattern DivideByZero(exp: Exp) {
   *   Div(exp)
   *   Div.rhs(div, val)
   *   find Zero(val)
   * }
   */
  private val divideByZeroPattern = GraphPattern(
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
            tempRhsVar,
            PathElementImpl(
              None,
              divRhsLink
            ),
            divType),
            CompositionConstraint(neg = false, PatternCall(transitive = false, Seq(VariableReference(tempRhsVar)), isZeroPattern))
        )
      )
    ),
    None
  )

  private val mulRhsLink = mulType("rhs")
  private val mulLhsLink = mulType("lhs")

  private val tempMulLhsVar = TemporaryVariable("lhs", Some(expType))
  private val tempMulRhsVar = TemporaryVariable("rhs", Some(expType))
  private val mulGPP = GraphPatternParameter("div", Some(mulType))
  /**
   * pattern IgnoreMultiplication(mul: Mul) {
   *   Mul.lhs.value(mul, lhs)
   *   lhs == 1
   * } or {
   *   Mul.rhs.value(mul, rhs)
   *   rhs == 1
   * } or {
   *   Mul.lhs.value(mul, lhs)
   *   lhs == 1.0
   * } or {
   *   Mul.rhs.value(mul, rhs)
   *   rhs == 1.0
   * }
   */
  private val ignoreMultiplicationPattern = GraphPattern(
    "IgnoreMultiplicationCalcLang",
    Seq(
      mulGPP
    ),
    Seq(
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(mulGPP),
            tempMulLhsVar,
            PathElementImpl(
              Some(
                PathElementImpl(
                  None,
                  intValueLink)
              ),
              mulLhsLink
            ),
            divType),
          CompareConstraint(EqualityCompareFeature(), tempMulLhsVar, IntegerLiteral(1))
        )
      ),
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(divGPP),
            tempMulRhsVar,
            PathElementImpl(
              Some(
                PathElementImpl(
                  None,
                  intValueLink)
              ),
              mulRhsLink
            ),
            divType),
          CompareConstraint(EqualityCompareFeature(), tempMulRhsVar, IntegerLiteral(1))
        )
      ),
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(mulGPP),
            tempMulLhsVar,
            PathElementImpl(
              Some(
                PathElementImpl(
                  None,
                  decValueLink)
              ),
              mulLhsLink
            ),
            divType),
          CompareConstraint(EqualityCompareFeature(), tempMulLhsVar, DecimalLiteral(1.0))
        )
      ),
      GraphPatternBody(
        Seq(
          PathExpressionConstraint(
            VariableReference(divGPP),
            tempMulRhsVar,
            PathElementImpl(
              Some(
                PathElementImpl(
                  None,
                  decValueLink)
              ),
              mulRhsLink
            ),
            divType),
          CompareConstraint(EqualityCompareFeature(), tempMulRhsVar, DecimalLiteral(1.0))
        )
      )
    ),
    None
  )

  private val isZeroTestInput_1 = Integr(0)
  private val isZeroTestInput_2 = Integr(1)
  private val isZeroTestInput_3 = Decimal(0.0)
  private val isZeroTestInput_4 = Decimal(1.0)
  private val isZeroTestInput_5 = Mul(Add(Integr(1), Decimal(1.0)), Div(Integr(2), Decimal(2.0)))

  test("Test is-zero pattern") {
    writeClass(isZeroPattern)

    val scope_zeroTestInput_1 = new TFQueryScope(isZeroTestInput_1)
    val scope_zeroTestInput_2 = new TFQueryScope(isZeroTestInput_2)
    val scope_zeroTestInput_3 = new TFQueryScope(isZeroTestInput_3)
    val scope_zeroTestInput_4 = new TFQueryScope(isZeroTestInput_4)
    val scope_zeroTestInput_5 = new TFQueryScope(isZeroTestInput_5)
    val isZeroTestInput_1_Matcher = EnginePool.getMatcher(generated.IsZeroCalcLang.instance(), scope_zeroTestInput_1, DifferentialReteBackendFactory.INSTANCE)
    val isZeroTestInput_2_Matcher = EnginePool.getMatcher(generated.IsZeroCalcLang.instance(), scope_zeroTestInput_2, DifferentialReteBackendFactory.INSTANCE)
    val isZeroTestInput_3_Matcher = EnginePool.getMatcher(generated.IsZeroCalcLang.instance(), scope_zeroTestInput_3, DifferentialReteBackendFactory.INSTANCE)
    val isZeroTestInput_4_Matcher = EnginePool.getMatcher(generated.IsZeroCalcLang.instance(), scope_zeroTestInput_4, DifferentialReteBackendFactory.INSTANCE)
    val isZeroTestInput_5_Matcher = EnginePool.getMatcher(generated.IsZeroCalcLang.instance(), scope_zeroTestInput_5, DifferentialReteBackendFactory.INSTANCE)

    println("isZeroTestInput_1 matches:")
    println(isZeroTestInput_1_Matcher.getAllMatches)
    assert(!isZeroTestInput_1_Matcher.getAllMatches.isEmpty)

    println("isZeroTestInput_2 matches:")
    println(isZeroTestInput_2_Matcher.getAllMatches)
    assert(isZeroTestInput_2_Matcher.getAllMatches.isEmpty)

    println("isZeroTestInput_3 matches:")
    println(isZeroTestInput_3_Matcher.getAllMatches)
    assert(!isZeroTestInput_3_Matcher.getAllMatches.isEmpty)

    println("isZeroTestInput_4 matches:")
    println(isZeroTestInput_4_Matcher.getAllMatches)
    assert(isZeroTestInput_4_Matcher.getAllMatches.isEmpty)

    println("isZeroTestInput_5 matches:")
    println(isZeroTestInput_5_Matcher.getAllMatches)
    assert(isZeroTestInput_5_Matcher.getAllMatches.isEmpty)

  }


  // div-by-zero-pattern test inputs

  private val divByZeroTestInput_1 = Div(Integr(1), Integr(0))
  private val divByZeroTestInput_2 = Div(Integr(1), Decimal(0.0))
  private val divByZeroTestInput_3 = Div(Integr(0), Integr(1))
  private val divByZeroTestInput_4 = Div(Integr(0), Decimal(1.0))
  private val divByZeroTestInput_5 = Mul(Add(Integr(0), Decimal(0.0)), Decimal(0.0))

  test("Test divide-by-zero pattern") {
    writeClass(isZeroPattern)
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
    assert(!divTestInput_1_Matcher.getAllMatches.isEmpty)
    println("divTestInput_2 matches:")
    println(divTestInput_2_Matcher.getAllMatches)
    assert(!divTestInput_2_Matcher.getAllMatches.isEmpty)


    println("divTestInput_3 matches:")
    println(divTestInput_3_Matcher.getAllMatches)
    assert(divTestInput_3_Matcher.getAllMatches.isEmpty)
    println("divTestInput_4 matches:")
    println(divTestInput_4_Matcher.getAllMatches)
    assert(divTestInput_4_Matcher.getAllMatches.isEmpty)
    println("divTestInput_5 matches:")
    println(divTestInput_5_Matcher.getAllMatches)
    assert(divTestInput_5_Matcher.getAllMatches.isEmpty)
  }

  private val ignoreMulTestInput_1 = Mul(Integr(1), Decimal(1.0))
  private val ignoreMulTestInput_2 = Mul(Decimal(1.0), Integr(1))
  private val ignoreMulTestInput_3 = Mul(Integr(5), Decimal(1.0))
  private val ignoreMulTestInput_4 = Mul(Integr(1), Decimal(5.0))
  private val ignoreMulTestInput_5 = Div(Integr(1), Decimal(1.0))
  private val ignoreMulTestInput_6 = Div(Decimal(1.0), Integr(1))
  private val ignoreMulTestInput_7 = Add(Integr(1), Decimal(1.0))
  private val ignoreMulTestInput_8 = Add(Decimal(1.0), Integr(1))

  test("Test ignore-mul pattern") {
    writeClass(ignoreMultiplicationPattern)


    val scope_ignoreMulTestInput_1 = new TFQueryScope(ignoreMulTestInput_1)
    val scope_ignoreMulTestInput_2 = new TFQueryScope(ignoreMulTestInput_2)
    val scope_ignoreMulTestInput_3 = new TFQueryScope(ignoreMulTestInput_3)
    val scope_ignoreMulTestInput_4 = new TFQueryScope(ignoreMulTestInput_4)
    val scope_ignoreMulTestInput_5 = new TFQueryScope(ignoreMulTestInput_5)
    val scope_ignoreMulTestInput_6 = new TFQueryScope(ignoreMulTestInput_6)
    val scope_ignoreMulTestInput_7 = new TFQueryScope(ignoreMulTestInput_7)
    val scope_ignoreMulTestInput_8 = new TFQueryScope(ignoreMulTestInput_8)
    val ignoreMulTestInput_1_Matcher = EnginePool.getMatcher(generated.IgnoreMultiplicationCalcLang.instance(), scope_ignoreMulTestInput_1, DifferentialReteBackendFactory.INSTANCE)
    val ignoreMulTestInput_2_Matcher = EnginePool.getMatcher(generated.IgnoreMultiplicationCalcLang.instance(), scope_ignoreMulTestInput_2, DifferentialReteBackendFactory.INSTANCE)
    val ignoreMulTestInput_3_Matcher = EnginePool.getMatcher(generated.IgnoreMultiplicationCalcLang.instance(), scope_ignoreMulTestInput_3, DifferentialReteBackendFactory.INSTANCE)
    val ignoreMulTestInput_4_Matcher = EnginePool.getMatcher(generated.IgnoreMultiplicationCalcLang.instance(), scope_ignoreMulTestInput_4, DifferentialReteBackendFactory.INSTANCE)
    val ignoreMulTestInput_5_Matcher = EnginePool.getMatcher(generated.IgnoreMultiplicationCalcLang.instance(), scope_ignoreMulTestInput_5, DifferentialReteBackendFactory.INSTANCE)
    val ignoreMulTestInput_6_Matcher = EnginePool.getMatcher(generated.IgnoreMultiplicationCalcLang.instance(), scope_ignoreMulTestInput_6, DifferentialReteBackendFactory.INSTANCE)
    val ignoreMulTestInput_7_Matcher = EnginePool.getMatcher(generated.IgnoreMultiplicationCalcLang.instance(), scope_ignoreMulTestInput_7, DifferentialReteBackendFactory.INSTANCE)
    val ignoreMulTestInput_8_Matcher = EnginePool.getMatcher(generated.IgnoreMultiplicationCalcLang.instance(), scope_ignoreMulTestInput_8, DifferentialReteBackendFactory.INSTANCE)

    println("mulTestInput_1 matches:")
    println(ignoreMulTestInput_1_Matcher.getAllMatches)
    assert(!ignoreMulTestInput_1_Matcher.getAllMatches.isEmpty)
    println("mulTestInput_2 matches:")
    println(ignoreMulTestInput_2_Matcher.getAllMatches)
    assert(!ignoreMulTestInput_2_Matcher.getAllMatches.isEmpty)
    println("mulTestInput_3 matches:")
    println(ignoreMulTestInput_3_Matcher.getAllMatches)
    assert(!ignoreMulTestInput_3_Matcher.getAllMatches.isEmpty)
    println("mulTestInput_4 matches:")
    println(ignoreMulTestInput_4_Matcher.getAllMatches)
    assert(!ignoreMulTestInput_4_Matcher.getAllMatches.isEmpty)

    println("mulTestInput_5 matches:")
    println(ignoreMulTestInput_5_Matcher.getAllMatches)
    assert(ignoreMulTestInput_5_Matcher.getAllMatches.isEmpty)
    println("mulTestInput_6 matches:")
    println(ignoreMulTestInput_6_Matcher.getAllMatches)
    assert(ignoreMulTestInput_6_Matcher.getAllMatches.isEmpty)
    println("mulTestInput_7 matches:")
    println(ignoreMulTestInput_7_Matcher.getAllMatches)
    assert(ignoreMulTestInput_7_Matcher.getAllMatches.isEmpty)
    println("mulTestInput_8 matches:")
    println(ignoreMulTestInput_8_Matcher.getAllMatches)
    assert(ignoreMulTestInput_8_Matcher.getAllMatches.isEmpty)
  }

}
