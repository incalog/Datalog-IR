package inca.frontend.objectoriented.util

import inca.frontend.objectoriented.core.{ClassDef, Name}

import scala.collection.MultiDict

case class ClassHierarchy(classes: Seq[ClassDef]) extends Iterable[(Seq[ClassDef], ClassDef, Seq[ClassDef])] {

  private lazy val classMap: Map[Name, ClassDef] = classes.map(c => c.name -> c).toMap

  private lazy val childMap: MultiDict[Name, ClassDef] = {
    MultiDict.from(classes.flatMap { c =>
      val parents = c.parentClassRefs.map { parentRef =>
        parentRef.name -> c
      }
      parents
    })
  }

  override def iterator: Iterator[(Seq[ClassDef], ClassDef, Seq[ClassDef])] =
    classes.map(c => (getDirectParents(c.name), c, getDirectChildren(c.name))).iterator

  def getClassDef(name: Name): ClassDef = {
    classMap(name)
  }

  def getDirectParents(name: Name): Seq[ClassDef] = {
    classMap(name).parentClassRefs.map(p => classMap(p.name))
  }

  def getDirectChildren(name: Name): Seq[ClassDef] = {
    childMap.get(name).toSeq
  }
}
