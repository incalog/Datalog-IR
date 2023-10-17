package inca.ir.typing

trait Resolvable[T] {
  var target: Option[T] = None

  def resolved(t: T, force: Boolean = true): this.type = {
    if (!force && this.target.nonEmpty)
      throw new IllegalArgumentException(s"May not overwrite resolved target.")
    this.target = Some(t)
    this
  }
}
