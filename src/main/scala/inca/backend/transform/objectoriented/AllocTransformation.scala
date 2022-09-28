package inca.backend.transform.objectoriented

import inca.backend.hints.ObjectHints
import inca.backend.ir.Datalog._
import inca.backend.ir.util.CollectVars
import inca.backend.transform.{Transformation, Transformer}
import inca.runtime.context.DataModel
import inca.util.Scala

import scala.meta.XtensionQuasiquoteTerm

/**
 * This transformation does the following things:
 * 1. Introduce an allocation counter in the AllocationRoot with the name `alloc` and initialize it with 0.
 * 2. Modify all affected methods that are neither an Allocation (leaf), nor a root to take an `allocIn` and `allcOut`
 *    parameter.
 * 3. Modify the embedded computation inside the Allocation (leafs) to use the `allocIn` argument as second parameter
 *    for the ObjectID creation. Increase the `allocIn` argument by one and assign the result to `allocOut` .
 */
object AllocTransformation extends Transformation {
  override def transformer(dataModel: DataModel): Transformer = new CountTransformer(
    ObjectHints.AllocationRoot,
    ObjectHints.Allocation,
    "alloc", "allocIn", "allocOut"
  ) {

    override def transformLeafPattern(leafPat: Pattern): Pattern = gensym.scoped  {
      gensym.register(CollectVars.transPattern(leafPat))

      val Pattern(vis, name, params, bodies) = leafPat
      val allocInName = gensym.fresh(inParamName)
      val allocOutName = gensym.fresh(outParamName)

      // wrap a Computed(Var, Evaluation) inside a lambda, that uses the dummy variable from the input call
      def transComputedEvaluation(lhs: Term, eval: Evaluation): Computed = {
        val orgFun = eval.code.tree
        val q"(..$params) => $f(..$args)" = orgFun
        val allocInArg = scala.meta.Term.Name(allocInName)
        val allocInParam = scala.meta.Term.Param(Nil, allocInArg, Some(TScalaInt.asScala), None)
        val newParams = params :+ allocInParam
        val newArgs = args :+ allocInArg
        val fun = q"(..$newParams) => $f(..$newArgs)"
        Computed(lhs, Evaluation(eval.evalArgs :+ Var(allocInName) -> TScalaInt, eval.resultType, Scala(fun)))
      }

      val newBodies = bodies.map { body =>
        Body(body.atoms.map {
          case Computed(lhs, eval : Evaluation) =>
            transComputedEvaluation(lhs, eval)
          case a => a
        } :+ Computed(
          Var(allocOutName), Evaluation(
            Seq(Var(allocInName) -> TScalaInt),
            TScalaInt,
            Scala(q"""(${scala.meta.Term.Name(allocInName)}: ${TScalaInt.asScala}) => ${scala.meta.Term.Name(allocInName)} + 1""")
          )
        ))
      }
      val newParams = params :+ Param(allocInName, TScalaInt) :+ Param(allocOutName, TScalaInt)
      Pattern(vis, name, newParams, newBodies).withHints(leafPat)
    }

    override def transformCall(call: Call, counterInVar: Var): (Var, Seq[Atom]) = {
      val Call(name, args, trans, neg) = call
      val hint = hintWithAdjustedFixedAdornment(call, Seq(true, false))
      if (isIgnoreCall(call)) {
        (counterInVar, Seq(
          Call(name, args :+ Var(gensym.fresh("_")) :+ Var(gensym.fresh("_")), trans, neg).withHints(hint)
        ))
      } else {
        val counterOutVar = Var(gensym.fresh(outParamName))
        (counterOutVar, Seq(
          Call(name, args :+ counterInVar :+ counterOutVar, trans, neg).withHints(hint)
        ))
      }
    }
  }
}
