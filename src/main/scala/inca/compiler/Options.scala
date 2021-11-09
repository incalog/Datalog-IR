package inca.compiler

import inca.backend.optimize._
import inca.backend.transform.Transformation
import org.eclipse.viatra.query.runtime.rete.matcher.ReteBackendFactory

trait Options {
  def optimizations: Seq[Optimization]

  def transformations: Seq[Transformation]

  def stopOnError: Boolean

  def stopOnWarning: Boolean

  def engine: ReteBackendFactory

  def withOptimizations(opts: Seq[Optimization]): Options
  def withTransformations(trans: Seq[Transformation]): Options
  def withEngine(eng: ReteBackendFactory): Options
}

object Options {

  val defaultOptimizations: Seq[Optimization] = Seq(
    EliminateNonproductiveRelations,
    InlineSimpleRelations,
    ConstantPropagation,
    EliminateAliases,
    EvalFusion,
    InferVarTypes,
    FoldConstantAtoms,
    EliminateNonproductiveRelations
  )
}