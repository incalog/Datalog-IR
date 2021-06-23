package inca.frontend.datalog.typechecker

import inca.frontend.datalog.syntax._

import scala.collection.immutable.MultiDict

trait TypeContext extends TypeIO {
  private var rels: Map[Name, RuleSig] = Map()
  private var vars: Map[Name, Type] = Map()
  private var callable: MultiDict[Name, Call.Target] = MultiDict()
  private var typeTarget: Map[Name, TData.Target] = Map()

  def scopedTypeContext[A](f: => A): A = {
    val oldRels = rels
    val oldVars = vars
    val oldCallable = callable
    val oldTypeTarget = typeTarget
    val a = f
    rels = oldRels
    vars = oldVars
    callable = oldCallable
    typeTarget = oldTypeTarget
    a
  }

  def bindVar(name: Name, ty: Type): Unit =
    vars += name -> ty

  def bindRelation(sig: RuleSig): Unit = {
    rels += sig.name -> sig
    callable += sig.name -> sig
  }

  def bindData(data: DataDef): Unit = {
    typeTarget += data.name -> data
    callable += data.name -> data
    data.constrs.foreach { c =>
      typeTarget += c.name -> c
      callable += c.name -> c
    }
  }

  def lookupRelation(name: Name): Option[RuleSig] =
    rels.get(name)

  def lookupVar(name: Name): Option[Type] =
    vars.get(name)

  def lookupCalled(name: Name): Option[Call.Target] =
    callable.get(name) match {
      case set if set.size == 1 =>
        Some(set.head)
      case set if set.size >= 2 =>
        error(s"Ambiguous call to $name with potential targets: $set", name)
        None
      case set if set.isEmpty =>
        error(s"Unbound name $name", name)
        None
    }

  def lookupType(name: Name): Option[TData.Target] =
    typeTarget.get(name) match {
      case Some(data) => Some(data)
      case None =>
        error(s"Unbound data type $name", name)
        None
    }

}