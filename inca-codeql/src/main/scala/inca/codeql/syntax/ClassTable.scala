package inca.codeql.syntax

import _root_.inca.ir.Name

case class ResolvedMember(owner: ClassDecl, declaration: MemberPredicateDecl)

class ClassTable(program: Program):
  val classes: Map[Name, ClassDecl] = program.classMap

  def directBaseClasses(clazz: ClassDecl): Seq[ClassDecl] =
    clazz.bases.flatMap {
      case QlType.EntityType(name) => classes.get(name)
      case _ => None
    }

  def ancestors(name: Name): Seq[ClassDecl] =
    def visit(current: Name, seen: Set[Name]): Seq[ClassDecl] =
      if seen.contains(current) then Seq.empty
      else classes.get(current).toSeq.flatMap { clazz =>
        clazz +: directBaseClasses(clazz).flatMap(base => visit(base.name, seen + current))
      }
    visit(name, Set.empty).groupBy(_.name).values.map(_.head).toSeq

  def descendants(name: Name): Seq[ClassDecl] =
    classes.values.filter(clazz => isSubtype(clazz.name, name)).toSeq

  def isSubtype(subtype: Name, supertype: Name): Boolean =
    subtype == supertype || ancestors(subtype).exists(_.name == supertype)

  def inheritedFields(name: Name): Seq[VariableDecl] =
    val clazz = classes(name)
    val inherited = directBaseClasses(clazz).flatMap(base => inheritedFields(base.name))
    (inherited ++ clazz.fields).foldLeft(Seq.empty[VariableDecl]) { (fields, field) =>
      if fields.exists(_.name == field.name) then fields else fields :+ field
    }

  def lookupMember(className: Name, memberName: Name): Option[ResolvedMember] =
    classes.get(className).flatMap { clazz =>
      clazz.members.find(_.name == memberName).map(ResolvedMember(clazz, _)).orElse {
        directBaseClasses(clazz).flatMap(base => lookupMember(base.name, memberName)).headOption
      }
    }

  def declaredMemberOwners(staticClass: Name, memberName: Name): Seq[ResolvedMember] =
    val inherited = lookupMember(staticClass, memberName).toSeq
    val overrides = descendants(staticClass).flatMap { clazz =>
      clazz.members.find(_.name == memberName).map(ResolvedMember(clazz, _))
    }
    (inherited ++ overrides).groupBy(_.owner.name).values.map(_.head).toSeq

  def hasInheritanceCycle: Boolean =
    def visit(name: Name, active: Set[Name], complete: Set[Name]): Boolean =
      if active.contains(name) then true
      else if complete.contains(name) then false
      else classes.get(name).exists { clazz =>
        directBaseClasses(clazz).exists(base => visit(base.name, active + name, complete + name))
      }
    classes.keys.exists(name => visit(name, Set.empty, Set.empty))
