package inca.compiler.options

import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.options.Options.{defaultOptimizations, defaultTransformations}

case class FunctionalOptions(optimizations: Seq[Optimization] = defaultOptimizations,
                             transformations: Seq[Transformation] = defaultTransformations,
                             stopOnError: Boolean = true,
                             stopOnWarning: Boolean = false) extends Options
