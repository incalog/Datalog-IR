package inca

import inca.CompilerOptions._
import inca.backend.optimize._
import inca.frontend.desugar.Desugarable
import inca.frontend.funext.{BoolOps, Cast, Enum, ForallExists, Foreach, IfThenElse, Match, Switch}
import inca.runtime.context.LanguageMetaInfo

case class CompilerOptions(languageMetaInfo: LanguageMetaInfo,
                           desugarables: Seq[Desugarable] = defaultDesugarables,
                           optimizations: Seq[Optimization] = defaultOptimizations)

object CompilerOptions {
  val defaultDesugarables = Seq(
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