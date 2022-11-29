package inca.backend.transform.objectoriented

import inca.backend.hints.MagicSetHints.{FixedAdornment, FixedAdornmentKey}
import inca.backend.hints.{MagicSetHints, ObjectHints, OptimizationHints}
import inca.backend.ir.Datalog._
import inca.backend.ir.util.CollectVars
import inca.backend.transform.{Transformation, Transformer}
import inca.runtime.aggregate.Aggregation
import inca.runtime.context.DataModel
import inca.util.Scala

import scala.meta.XtensionQuasiquoteTerm

case class MaxAgg() extends Aggregation[Int] {
  override val name: String = "max"
  override def init: Int = Int.MinValue
  override def join(v1: Int, v2: Int): Int = v1.max(v2)
  //override def unjoin(v1: Int, v2: Int): Int = v1 - v2
  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
  override val hasUnjoin: Boolean = false
}

/**
 * This transformation performs several tasks:
 * 1. Introduce a timestamp counter in the FieldRoot with the name `ts` and initialize it with 0.
 * 2. Modify all affected methods that are neither a Field (leaf), nor a root to take an `tsIn` and `tsOut` param.
 * 3. Modify all Fields (leafs) to take an additional timestamp argument (`ts`).
 * 3. Introduce a filter pattern for each Field (leaf) that has the same parameters as the corresponding field with one
 *    additional parameter `tsMax`. This pattern allows filtering based the timestamp.
 * 3. Modify all calls according to the following scheme based on their target:
 *    - target: Field (Get) =>
 *        - Insert a max aggregation over the filtered field pattern (only timestamp smaller than `tsIn`)
 *        - Insert the timestamp received as aggregation result as the last parameter of the original call
 *    - target: Field (Set) =>
 *        - Insert the `tsIn` argument as last argument of the call
 *        - Increase `tsIn` by 1 and return the result as `tsOut`
 *    - target: Call (Ignore) =>
 *        - Insert two dummy arguments (one for `tsIn` and one for `tsOut`)
 *    - target: Call (Ignore) =>
 *        - Insert two arguments, one for `tsIn` and one for `tsOut`
 */
object FieldTransformation extends Transformation {
  override def transformer(dataModel: DataModel): Transformer = new CountTransformer(
    ObjectHints.FieldRoot,
    ObjectHints.Field,
    "ts", "tsIn", "tsOut"
  ) {

    private def isFieldGetCall(call: Call): Boolean =
      call.hasHint(ObjectHints.FieldGetKey)

    private def isFieldSetCall(call: Call): Boolean =
      call.hasHint(ObjectHints.FieldSetKey)

    private def filterPatternName(fieldPatName: String): String = {
      fieldPatName + "$" + "Filter"
    }

    /**
     * Find the biggest timestamp in the field pattern, that is smaller than maxTs. In a first step, the filtered field
     * pattern is used to eliminate all entries with a timestamp bigger than maxTs. The maximum aggregation is then
     * performed on the filtered pattern on column 2 (timestamp column) with the specified object.
     * @param fieldPatName The name of the field pattern to find the timestamp for.
     * @param obj The object to get the field for.
     * @param outVar The output timestamp calculated by the aggregation.
     * @param maxTs The upperbound for the timestamps to consider.
     * @return The Computed atom.
     */
    private def maxAgg(fieldPatName: String, args: Seq[Term], outVar: Var, maxTs: Var): Computed =
      Computed(
        outVar,
        CustomAggregation(
          TScalaInt,
          Some("Maximum aggregation"),
          Scala(q"""new inca.backend.transform.objectoriented.MaxAgg()"""),
          filterPatternName(fieldPatName),
          args :+ Var(gensym.fresh("_")) :+ maxTs, // args + ts + maxTs
          args.size // aggregate over ts, not maxTs
        )
      )

    private def generateFilterPattern(fieldPat: Pattern): Pattern = gensym.scoped {
      val tsParams = Seq(
        Param(gensym.fresh(rootParamName), TScalaInt),
        Param(gensym.fresh(rootParamName + "Max"), TScalaInt)
      )

      val fieldArgs = fieldPat.params.map(p => Var(p.name))
      val additionalArgs = tsParams.map(p => Var(p.name))

      val (tsTerm, tsParam) = createScalaTermAndParam(tsParams.head.name, tsParams.head.typ)
      val (tsMaxTerm, tsMaxParam) = createScalaTermAndParam(tsParams.last.name, tsParams.last.typ)

      val body = Body(Seq(
        Call(fieldPat.name, fieldArgs :+ additionalArgs.head)
          .addHint(MagicSetHints.IgnoreCall)
          .addHint(MagicSetHints.FixedAdornment(fieldArgs.map(_ => true) :+ true)),
        Computed(
          True,
          Evaluation(
            tsParams.map(p => Var(p.name) -> p.typ),
            TScalaBoolean,
            Scala(q"($tsParam, $tsMaxParam) => $tsTerm < $tsMaxTerm")
          )
        )
      ))

      Pattern(fieldPat.vis, filterPatternName(fieldPat.name), fieldPat.params ++ tsParams, Seq(body))
    }

    override def generateAdditionalPattern(leafPattern: Seq[Pattern], rootPattern: Set[Pattern], affectedPattern: Set[Pattern], unchangedPattern: Set[Pattern]): Seq[Pattern] = {
      leafPattern.map(generateFilterPattern)
    }

    override def transformLeafPattern(leafPat: Pattern, affectedPattern: Set[Pattern]): Pattern = gensym.scoped {
      gensym.register(CollectVars.transPattern(leafPat))

      /*if (leafPat.params.size != 2) {
        throw new IllegalArgumentException(s"Field pattern ${leafPat.name} requires exactly two parameters!")
      }*/

      if (leafPat.bodies.nonEmpty) {
        throw new IllegalArgumentException(s"Field pattern ${leafPat.name} must not have a body!")
      }

      val tsParam = Param(gensym.fresh(rootParamName), TScalaInt)
      Pattern(leafPat.vis, leafPat.name, leafPat.params :+ tsParam, Seq())
        .withHints(leafPat)
        .addHint(OptimizationHints.NoInline)
    }

    override def transformCall(call: Call, tsInVar: Var): (Var, Seq[Atom]) = {
      val Call(name, args, trans , neg) = call

      // If the call targets a field we want to either insert an aggregation in case of a Get or tsIn to the call in
      // case of a set.
      if (isFieldGetCall(call)) {
        val hint = hintWithAdjustedFixedAdornment(call, args.size, Seq(true))
        val tsMaxVar = Var(gensym.fresh(rootParamName + "Max"))
        (tsInVar, Seq(
          maxAgg(name, args.head +: args.tail.map(_ => Var(gensym.fresh("_"))), tsMaxVar, tsInVar),
          Call(name, args :+ tsMaxVar, trans, neg)
            .withHints(hint)
            .addHint(MagicSetHints.IgnoreCall)
        ))
      } else if (isFieldSetCall(call)) {
        val hint = hintWithAdjustedFixedAdornment(call, args.size, Seq(true))
        val (tsOutVar, incComp) = incCounter(tsInVar)
        (tsOutVar, Seq(
          Call(name, args :+ tsInVar, trans, neg).withHints(hint),
          incComp
        ))
      } else {
        super.transformCall(call, tsInVar)
      }
    }
  }
}
