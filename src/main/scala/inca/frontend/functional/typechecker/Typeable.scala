package inca.frontend.functional.typechecker

import inca.frontend.functional.core.Type

trait Typeable {
  var typ: Option[Type] = None

  def typed(ty: Type): this.type = {
    if (this.typ.nonEmpty)
      throw new IllegalArgumentException(s"May not overwrite annotated type.")
    this.typ = Some(ty)
    this
  }

  def mtyped(ty: Option[Type]): this.type = {
    this.typ = this.typ.orElse(ty)
    this
  }

  def orTyped(ty: Type): this.type = {
    this.typ = this.typ.orElse(Some(ty))
    this
  }
}