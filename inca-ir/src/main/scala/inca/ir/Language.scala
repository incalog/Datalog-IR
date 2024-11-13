package inca.ir

case class Language(features: Set[BaseIR]):
  def +(feature: BaseIR): Language = Language(features + feature)

  def --(features: Set[BaseIR]): Language = Language(this.features -- features)

  def ++(features: Set[BaseIR]): Language = Language(this.features ++ features)

  def includes(that: Language): Boolean = that.features.subsetOf(this.features)

  override def toString: String = s"Language(${features.map(_.name).mkString(", ")})"

object Language:
  val Datalog: Language = new Language(Set(new BaseIR {}))

  def apply(features: BaseIR*) = new Language(Set(features*) + new BaseIR {})

