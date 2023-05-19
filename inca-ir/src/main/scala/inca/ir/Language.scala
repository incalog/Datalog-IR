package inca.ir

case class Language(features: Set[IR]):
  def +(feature: IR): Language = Language(features + feature)
  def --(features: Set[IR]): Language = Language(this.features -- features)
  def includes(that: Language): Boolean = that.features.subsetOf(this.features)

object Language:
  val Datalog: Language = new Language(Set())
  def apply(features: IR*) = new Language(Set(features:_*))

