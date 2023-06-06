package inca.ir.extensions

import inca.ir.*

sealed trait TLinked extends Type
//case object TAnyLinked extends TLinked
case class TNode(name: Name) extends TLinked
case class TList(contained: TLinked) extends TLinked

sealed trait Link
case object ParentLink extends Link
case object NextLink extends Link
case object SizeLink extends Link
case class NamedLink(node: TNode, field: Name) extends Link

case class HasType(t: Term, typ: Type) extends Atom
case class NotHasType(t: Term, typ: Type) extends Atom
case class Path(src: Term, srcTy: Type, link: Link, trg: Term, trgTy: Type) extends Atom
case class NoPath(t: Term, ty: Type, link: Link, termIsSource: Boolean) extends Atom

trait ExtensionalTreesIR extends BaseIR:
  override val name: String = "ExtensionalTrees"
  override def language: Language = super.language + new ExtensionalTreesIR {}
  override def requires: Language = Language()
