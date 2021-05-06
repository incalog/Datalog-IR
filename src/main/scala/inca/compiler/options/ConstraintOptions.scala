package inca.compiler.options

import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.options.Options.{defaultDesugarables, defaultOptimizations}
import inca.frontend.constraint.desugar.Desugarable

case class ConstraintOptions(optimizations: Seq[Optimization] = defaultOptimizations,
                             transformations: Seq[Transformation] = Seq(),
                             desugarables: Seq[Desugarable] = defaultDesugarables,
                             stopOnError: Boolean = true,
                             stopOnWarning: Boolean = false) extends Options
