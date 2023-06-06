package inca.ir.typing

trait Typeable[T]:
  var typ: Option[T] = None

  def typed(ty: T): this.type = {
    if (this.typ.nonEmpty)
      throw new IllegalArgumentException(s"May not overwrite annotated type.")
    this.typ = Some(ty)
    this
  }

  def mtyped(ty: Option[T]): this.type = {
    this.typ = this.typ.orElse(ty)
    this
  }

  def orTyped(ty: T): this.type = {
    this.typ = this.typ.orElse(Some(ty))
    this
  }
