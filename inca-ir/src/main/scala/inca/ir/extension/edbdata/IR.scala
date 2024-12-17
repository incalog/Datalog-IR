package inca.ir.extension.edbdata

import inca.ir.*
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.string.TString
import truechange.{AnyType, JavaLitType, ListType, NamedLink, NodeMetaInfo, NothingType, OptionType, RefType, SortType}

import scala.collection.mutable.ListBuffer

trait EdbDataModuleEntry extends ModuleEntry

case class EdbNodeDefinition(name: Name, sup: Option[Name] = None) extends EdbDataModuleEntry:
  def withName(name: String): EdbNodeDefinition = this.copy(name = Name(name))

object EdbNodeDefinition:
  def apply(name: Name, sup: Name): EdbNodeDefinition = new EdbNodeDefinition(name, Some(sup))

case class EdbFieldDefinition(node: Name, field: Name, ty: EdbType) extends EdbDataModuleEntry:
  override val name: Name = edbFieldName(node, field)
  def withName(name: String): EdbFieldDefinition = this.copy(field = Name(name))

def edbFieldName(node: Name, field: Name): Name = Name(node.name + ":" + field.name)

trait EdbType extends Type

/** Type of atomic values in the EDB */
case class TEdbValue(ty: Type) extends EdbType

/** Nominal type of tree nodes in the EDB */
case class TEdbNode(name: Name) extends EdbType

/** Type of lists in the EDB */
case class TEdbList(ty: EdbType) extends EdbType


case class LookupEdbType(ty: EdbType) extends Term:
  override def vars: Seq[Var] = Seq()

case class NotInEdbType(t: Term, ty: EdbType) extends Atom:
  override def vars: Seq[Var] = t.vars

case class UndefEdbType(ty: EdbType) extends Atom:
  override def vars: Seq[Var] = Seq()

case class LookupEdbField(src: Term, link: Link) extends Term:
  override def vars: Seq[Var] = src.vars

object LookupEdbField:
  def apply(t: Term, field: Name): LookupEdbField = new LookupEdbField(t, Link.Field(field))

case class UndefEdbField(src: Term, link: Link) extends Atom:
  override def vars: Seq[Var] = src.vars

case class UndefEdbFieldInverse(srcTy: EdbType, link: Link, trg: Term) extends Atom:
  override def vars: Seq[Var] = trg.vars

def EdbDeconstruct(t: Term, node: Name, fields: (String, Term)*): Seq[Atom] =
  Eq(Cast(t, TEdbNode(node)), LookupEdbType(TEdbNode(node))) +:
  fields.map((field, a) =>
    Eq(a, LookupEdbField(Cast(t, TEdbNode(node)), Name(field)))
  )

enum Link:
  case Field(name: Name)
  case Parent
  case Children
  case Next
  case Prev
  case Size
  case First
  case Last

object EdbDataModuleEntry:
  def fromNodeMetaInfos(nodes: Seq[NodeMetaInfo]): Seq[EdbDataModuleEntry] =
    nodes.flatMap(fromNodeMetaInfo)

  def fromNodeMetaInfos(node: NodeMetaInfo, nodes: NodeMetaInfo*): Seq[EdbDataModuleEntry] =
    fromNodeMetaInfo(node) ++ nodes.flatMap(fromNodeMetaInfo)

  def fromNodeMetaInfo(node: NodeMetaInfo): Seq[EdbDataModuleEntry] =
    if (node.superSorts.size > 1)
      throw new IllegalArgumentException(s"EDB nodes with multiple super sorts not currently supported")

    val nodeName = EdbType.fromTruechangeSortType(node.sort).name
    val buf = ListBuffer.empty[EdbDataModuleEntry]
    val sup = node.superSorts.headOption.map(EdbType.fromTruechangeSortType(_).name)
    buf += EdbNodeDefinition(nodeName, sup)
    node.links.foreach { case (NamedLink(linkName), trgTy) =>
      buf += EdbFieldDefinition(nodeName, Name(linkName), EdbType.fromTruechangeType(trgTy))
    }
    node.litLinks.foreach { case (NamedLink(linkName), trgTy) =>
      buf += EdbFieldDefinition(nodeName, Name(linkName), EdbType.fromTruechangeLitType(trgTy))
    }
    buf.toList

object EdbType:
  def fromTruechangeType(ty: truechange.Type): EdbType = ty match
    case st: SortType => fromTruechangeSortType(st)
    case ListType(elTy) => TEdbList(fromTruechangeType(elTy))
    case _ => throw new UnsupportedOperationException(s"Cannot convert $ty to EdbType")

  def fromTruechangeSortType(ty: truechange.SortType): TEdbNode =
    val name = ty.name
    TEdbNode(Name(name))


  def fromTruechangeLitType(lty: truechange.LitType): EdbType = lty match
    case JavaLitType(cl) =>
      if (cl == classOf[Int] || cl == classOf[Integer])
        TEdbValue(TInt)
      else if (cl == classOf[Double] || cl == classOf[java.lang.Double])
        TEdbValue(TDouble)
      else if (cl == classOf[String])
        TEdbValue(TString)
      else
        throw new UnsupportedOperationException(s"Cannot convert $lty to TEdbValue")
    case _ => throw new UnsupportedOperationException(s"Cannot convert $lty to TEdbValue")

object IR extends IR

trait IR extends BaseIR:
  override val name: String = "EdbTrees"
  override def language: Language = super.language + new IR {}
  override def requires: Language = Language()
