package inca

import inca.CompilerOptions._
import inca.backend.optimize._
import inca.frontend.desugar.Desugarable
import inca.frontend.extensions.{BoolOps, Cast, DataOpCall, Enum, ForallExists, Foreach, IfThenElse, Match, Switch}
import inca.runtime.context.LanguageMetaInfo

case class CompilerOptions(languageMetaInfo: LanguageMetaInfo,
                           desugarables: Seq[Desugarable] = defaultDesugarables,
                           optimizations: Seq[Optimization] = defaultOptimizations)

object CompilerOptions {
  val defaultDesugarables = Seq(
    DataOpCall,
    BoolOps,
    Cast,
    Enum,
    ForallExists,
    Foreach,
    IfThenElse,
    Match,
    Switch)

  val defaultOptimizations = Seq(
    ConstantPropagation,
    EliminateAliases,
    InferVarTypes,
    FoldConstantConstraints
  )
}