package inca.backend.transform.objectoriented

import inca.backend.hints.{MagicSetHints, ObjectHints, OptimizationHints}
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog._
import inca.backend.ir.util.CollectVars
import inca.backend.transform.objectoriented.transformer.{EclipseMapCountTransformer, MapCountTransformer}
import inca.backend.transform.{Transformation, Transformer}
import inca.runtime.context.DataModel
import inca.util.Scala

import scala.meta.XtensionQuasiquoteTerm

// TODO: Refactor this
object EclipseStructuralMutationTransformation extends Transformation {
  override def transformer(dataModel: DataModel): Transformer = new EclipseMapCountTransformer(
    ObjectHints.FieldRootKey,
    ObjectHints.FieldKey,
    "ts", "tsIn", "tsOut"
  ) {
    private var immutableFieldPattern: Seq[Name] = Seq()

    private def isImmutableField(pattern: Pattern): Boolean =
      pattern.hints.get(ObjectHints.FieldKey) match {
        case Some(ObjectHints.Field(true)) => true
        case _ => false
      }

    private def isFieldGetCall(call: Call): Boolean =
      call.hasHint(ObjectHints.FieldGetKey)

    private def isFieldSetCall(call: Call): Boolean =
      call.hasHint(ObjectHints.FieldSetKey)

    override def insertCounter(pattern: Seq[Pattern]): Seq[Pattern] = {
      immutableFieldPattern = pattern.filter(isImmutableField).map(_.name)
      super.insertCounter(pattern)
    }

    override def transformLeafPattern(leafPat: Datalog.Pattern, affectedPattern: Set[Datalog.Pattern]): Datalog.Pattern = {
      gensym.register(CollectVars.transPattern(leafPat))

      // Do nothing for immutable fields
      if (isImmutableField(leafPat))
        return leafPat

      if (leafPat.bodies.nonEmpty)
        throw new IllegalArgumentException(s"Field pattern ${leafPat.name} must not have a body!")

      val tsParam = Param(gensym.fresh(rootParamName), TScalaInt)
      Pattern(leafPat.vis, leafPat.name, leafPat.params :+ tsParam, Seq())
        .withHints(leafPat)
        .addHint(OptimizationHints.NoInline)
    }

    override def transformCall(call: Call, tsInVar: Var): (Var, Seq[Atom]) = {
      val Call(name, args, trans, neg) = call

      if (immutableFieldPattern.contains(name)) {
        if (isFieldGetCall(call))
          (tsInVar, Seq(call.addHint(MagicSetHints.IgnoreCall)))
        else
          (tsInVar, Seq(call))
      } else if (isFieldGetCall(call)) {
        val hint = hintWithAdjustedFixedAdornment(call, args.size, Seq(true))
        val (tsMaxVar, readMaxTs) = getCounter(tsInVar, args.head, name)

        (tsInVar, Seq(
          readMaxTs,
          Call(name, args :+ tsMaxVar, trans, neg)
            .withHints(hint)
            .addHint(MagicSetHints.IgnoreCall)
        ))
      } else if (isFieldSetCall(call)) {
        val hint = hintWithAdjustedFixedAdornment(call, args.size, Seq(true))
        val fieldSetHint = call.hints(ObjectHints.FieldSetKey).asInstanceOf[ObjectHints.FieldSet]

        if (fieldSetHint.fixedTimestamp.isEmpty) {
          // Read the old counter
          val (oldTs, readOldTs) = getCounter(tsInVar, args.head, name)
          // Increase it by one
          val (newTs, incTs) = incIntValue(oldTs)
          // Update structural counter
          val (tsOutVar, upCounter) = updateCounter(tsInVar, newTs, args.head, name)
          (tsOutVar, Seq(
            readOldTs, incTs, upCounter,
            Call(name, args :+ newTs, trans, neg).withHints(hint),
          ))
        } else {
          val ts = fieldSetHint.fixedTimestamp.get
          val tsVar = Var(gensym.fresh(outParamName))
          val tsComp = Computed(tsVar, Evaluation(Seq(), TScalaInt, Scala(q"() => $ts")))
          val (tsOutVar, upCounter) = updateCounter(tsInVar, tsVar, args.head, name)
          (tsOutVar, Seq(
            tsComp, upCounter,
            Call(name, args :+ tsVar, trans, neg).withHints(hint)
          ))
        }
      } else {
        super.transformCall(call, tsInVar)
      }
    }
  }
}
