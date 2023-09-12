package inca.ir.typing

import scala.reflect.ClassTag

trait Typeable[T]:
  var typ: Option[T] = None

  def typed(ty: T, force: Boolean = false): this.type = {
    if (!force && this.typ.nonEmpty)
      throw new IllegalArgumentException(s"May not overwrite annotated type ${this.typ} for $this.")
    this.typ = Some(ty)
    this
  }

  def isTypeOf[A <: T](implicit tag: ClassTag[A]): Boolean = {
    this.typ.exists(tag.runtimeClass.isInstance)
  }

  def mtyped(ty: Option[T]): this.type = {
    this.typ = this.typ.orElse(ty)
    this
  }

  def orTyped(ty: T): this.type = {
    this.typ = this.typ.orElse(Some(ty))
    this
  }
