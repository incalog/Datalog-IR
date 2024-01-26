package inca.ir.typing.deps

import inca.ir.{Hint, HintKey, Name, Param, Relation}

import scala.annotation.targetName

enum FunctionalOriginPart:
  case Parameter(rel: Relation, p: Param)
  case Variable(name: Name)

/** The origin parts together uniquely determine the annotated term */
case class FunctionalOrigin(parts: Set[FunctionalOriginPart])
/** Each of the origins uniquely determines the annotated term */
case class FunctionalOrigins(origins: Set[FunctionalOrigin]):
  @targetName("plus")
  def +(origin: FunctionalOrigin): FunctionalOrigins =
    FunctionalOrigins(origins + origin)


/** For relations. The `from` columns uniquely determine all the `to` columns. */
case class FunctionalDependency(from: Seq[Param], to: Seq[Param])

object FunctionalDependencyHint extends HintKey[FunctionalDependencyHint]:
  def apply(dep: FunctionalDependency, deps: FunctionalDependency*): FunctionalDependencyHint =
    new FunctionalDependencyHint(dep +: deps)
  def apply(from: Seq[Param], to: Seq[Param]): FunctionalDependencyHint =
    new FunctionalDependencyHint(Seq(FunctionalDependency(from, to)))
case class FunctionalDependencyHint(deps: Seq[FunctionalDependency]) extends Hint:
  override def key: HintKey[_] = FunctionalDependencyHint
