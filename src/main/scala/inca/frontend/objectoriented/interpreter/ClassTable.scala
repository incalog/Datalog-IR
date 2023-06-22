package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.core.{ClassDef, FieldDef, Name}

case class ClassTable(classDefs: Seq[ClassDef]) {
  private val classes: Map[Name, ClassDef] = classDefs.map(c => c.name -> c).toMap

  def lookup(className: Name): Option[ClassDef] = classes.get(className)

  def isSubclassOf(className: Name, ofClass: Name): Boolean = {
    lookup(className) match {
      case Some(classDef) =>
        classDef.name == ofClass || classDef.parentClassRefs.exists(ref => isSubclassOf(ref.name, ofClass))
      case None =>
        throw new ClassNotFoundException(s"Did not find class $className")
    }
  }

  def isCaseClass(className: Name): Boolean = {
    lookup(className) match {
      case Some(classDef) => classDef.isCaseClass
      case None => throw new ClassNotFoundException(s"Did not find class $className")
    }
  }

  def transitiveCollectFields(className: Name): Seq[FieldDef] = {
    lookup(className) match {
      case Some(classDef) => classDef.fields ++ classDef.parentClassRefs.flatMap(ref => transitiveCollectFields(ref.name))
      case None => throw new ClassNotFoundException(s"Did not find class $className")
    }
  }
}
