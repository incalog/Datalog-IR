package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.core.{ClassDef, FieldDef, Name}

case class ClassTable(classDefs: Seq[ClassDef]) {
  private val classes: Map[Name, ClassDef] = classDefs.map(c => c.name -> c).toMap

  def lookup(className: Name): ClassDef = classes.get(className) match {
    case Some(classDef) => classDef
    case None => throw new IllegalArgumentException(s"Unresolved class $className")
  }

  def isSubclassOf(className: Name, ofClass: Name): Boolean = {
    val classDef = lookup(className)
    classDef.name == ofClass || classDef.parentClassRefs.exists(ref => isSubclassOf(ref.name, ofClass))
  }

  def transitiveCollectFields(className: Name): Seq[FieldDef] = {
    val classDef = lookup(className)
    classDef.fields ++ classDef.parentClassRefs.flatMap(ref => transitiveCollectFields(ref.name))
  }
}
