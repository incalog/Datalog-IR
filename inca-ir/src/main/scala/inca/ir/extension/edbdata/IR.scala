package inca.ir.extension.edbdata

import inca.ir.*

trait EdbDataModuleEntry extends ModuleEntry

case class EdbNodeDefinition(name: Name, sup: Option[Name] = None) extends EdbDataModuleEntry:
  def withExtendedName(suffix: String): EdbNodeDefinition = this.copy(name = Name(name.name + suffix))
  override def toString: String = s"""edb node $name""" + sup.map(" extends " + _).getOrElse("")
object EdbNodeDefinition:
  def apply(name: Name, sup: Name): EdbNodeDefinition = new EdbNodeDefinition(name, Some(sup))

case class EdbFieldDefinition(node: Name, field: Name, ty: EdbType) extends EdbDataModuleEntry:
  override val name: Name = edbFieldName(node, field)
  def withExtendedName(suffix: String): EdbFieldDefinition = this.copy(field = Name(field.name + suffix))
  override def toString: String = s"""edb field $node.$field: $ty"""
def edbFieldName(node: Name, field: Name): Name = Name(node.name + ":" + field.name)

trait EdbType extends Type

/** Type of atomic values in the EDB */
case class TEdbValue(ty: Type) extends EdbType:
  override def toString: String = s"$ty@edb"
/** Nominal type of tree nodes in the EDB */
case class TEdbNode(name: Name) extends EdbType:
  override def toString: String = s"$name@edb"
/** Type of lists in the EDB */
case class TEdbList(ty: EdbType) extends EdbType:
  override def toString: String = s"List[$ty]@edb"


case class LookupEdbType(ty: EdbType) extends Term:
  override def vars: Seq[Var] = Seq()
  override def toString: String = s"edb[$ty]"
case class UndefEdbType(ty: EdbType) extends Atom:
  override def vars: Seq[Var] = Seq()
  override def toString: String = s"undef edb[$ty]"

case class LookupEdbField(t: Term, link: Link) extends Term:
  override def vars: Seq[Var] = t.vars
  override def toString: String = s"$t.$link"
object LookupEdbField:
  def apply(t: Term, field: Name): LookupEdbField = new LookupEdbField(t, Link.Field(field))
case class UndefEdbField(t: Term, link: Link) extends Atom:
  override def vars: Seq[Var] = t.vars
  override def toString: String = s"undef $t.$link"



enum Link:
  case Field(name: Name)
  case Parent
  case Children
  case Next
  case Prev
  case Size
  case First
  case Last

object IR extends IR
trait IR extends BaseIR:
  override val name: String = "EdbTrees"
  override def language: Language = super.language + new IR {}
  override def requires: Language = Language()
