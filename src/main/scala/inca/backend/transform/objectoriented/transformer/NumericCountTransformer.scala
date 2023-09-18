package inca.backend.transform.objectoriented.transformer

import inca.backend.ir.Datalog._
import inca.util.Scala

import scala.meta.XtensionQuasiquoteTerm

/**
 * Should be applied after demand transformation.
 *
 * A CountTransformer initialized a counter with the value 0 and name `rootParamName` in all bodies of a root pattern.
 * The root pattern is defined by the `rootPatternHint` [transformRootPattern].
 *
 * All pattern that directly or indirectly call a leaf pattern (defined by the `leafPatternHint`) are calculated up to
 * the root pattern. These pattern are called affected pattern. Affected pattern are modified to take two additional
 * parameter: an input counter and an output counter [transformAffectedPattern].
 *
 * All calls that target an affected pattern, a leaf pattern or a root pattern are modified to propagate the input and
 * output counter [transformCall].
 *
 * All unaffected pattern are transformed to insert "don't care" variables when calling an affected pattern.
 *
 * The leaf pattern is modified based on behaviour defined by a concrete implementation of this class
 * [transformChildPattern].
 */
abstract class NumericCountTransformer(override val rootPatternHint: String,
                                       override val leafPatternHint: String,
                                       override val rootParamName: String,
                                       override val inParamName: String,
                                       override val outParamName: String)
  extends CountTransformer(
    rootPatternHint,
    leafPatternHint,
    rootParamName,
    inParamName,
    outParamName
  )(TScalaInt) {

  override private[objectoriented] def produceInitialCounter(): (Term, Seq[Atom]) = {
    (Constant(IntLiteral(1)), Seq())
  }

  /**
   * Helper method to increase the counter.
   * @param counterIn The counter to increase.
   * @return Tuple with the output variable and the corresponding Computed to increase the counter.
   */
  private[objectoriented] def incCounter(counterIn: Var): (Var, Computed) = {
    val counterOutVar = Var(gensym.fresh(outParamName))
    val (counterInArg, counterInParam) = createScalaTermAndParam(inParamName, TScalaInt)
    (counterOutVar, Computed(
      counterOutVar, Evaluation(Seq(counterIn -> TScalaInt), TScalaInt, Scala(q"($counterInParam) => $counterInArg + 1"))
    ))
  }
}
