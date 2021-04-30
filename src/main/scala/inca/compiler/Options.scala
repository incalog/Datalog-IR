package inca.compiler

import inca.backend.optimize._
import inca.compiler.Options.{defaultDesugarables, defaultOptimizations}
import inca.frontend.desugar.Desugarable
import inca.frontend.extensions
import inca.runtime.context.DataModel

case class Options(optimizations: Seq[Optimization] = defaultOptimizations,
                   desugarables: Seq[Desugarable] = defaultDesugarables,
                   stopOnError: Boolean = true,
                   stopOnWarning: Boolean = false) {
}

object Options {

  val defaultOptimizations: Seq[Optimization] = Seq(
    ConstantPropagation,
    EliminateAliases,
    InferVarTypes,
    FoldConstantConstraints
  )

  val defaultDesugarables: Seq[Desugarable] = Seq(
    extensions.boolOps.Desugaring,
    extensions.evalCall.Desugaring,
    extensions.forallExists.Desugaring,
    extensions.foreach.Desugaring,
    extensions.ifThenElse.Desugaring,
    extensions.match_.Desugaring,
    extensions.switch_.Desugaring
  )
}