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

      val (allocOut, incComp) = incCounter(Var(allocInName))

      val newBodies = bodies.map { body =>
        Body(body.atoms.map {
          case Computed(lhs, eval : Evaluation) =>
            transComputedEvaluation(lhs, eval)
          case a => a
        } :+ incComp)
      }
      val newParams = params :+ Param(allocInName, TScalaInt) :+ Param(allocOut.name, TScalaInt)
      Pattern(vis, name, newParams, newBodies).withHints(leafPat)
    }
  }
}
