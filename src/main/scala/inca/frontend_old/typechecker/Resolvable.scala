package inca.frontend_old.typechecker

trait Resolvable[T] {
  var target: Option[T] = None

  def resolved(t: T): this.type = {
    if (this.target.nonEmpty)
      throw new IllegalArgumentException(s"May not overwrite resolved target.")
    this.target = Some(t)
    this
  }
}
