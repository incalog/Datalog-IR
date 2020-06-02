package org.inca.analyzedLangs

import org.inca.incer.IncrementalIndex

@IncrementalIndex
case class ClassDeclaration(name: String, isFinal: Boolean, members: List[ClassMember])

@IncrementalIndex
trait ClassMember

@IncrementalIndex
case class FieldDeclaration(name: String, visibility: Visibility) extends ClassMember {}

@IncrementalIndex
trait Visibility

@IncrementalIndex
case class PublicVisibility() extends Visibility

@IncrementalIndex
case class ProtectedVisibility() extends Visibility

@IncrementalIndex
case class PrivateVisibility() extends Visibility

trait Primitive {
  val value: Any
}

case class BooleanConstant(value: Boolean) extends Primitive

case class IntegerConstant(value: Int) extends Primitive

case class LongConstant(value: Long) extends Primitive

