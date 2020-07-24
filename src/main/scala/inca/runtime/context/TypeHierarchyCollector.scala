package inca.runtime.context

import scala.collection.immutable.{List, Map, Nil, Set}

object TypeHierarchyCollector {
  // what is the entry point?
  def apply(string: String): Map[Class[_], Set[Class[_]]] = {
    // collect all classes
    val classes = Set()
    classes.map { clazz => (clazz, superTypesOf(clazz).toSet) }.toMap
  }

  def superTypesOf(clazz: Class[_]): List[Class[_]] = {
    // not object, serializable, equals, hashable, diffabl
    val allSuper = allSuperTypesOf(clazz)
    allSuper.filter { c =>
      val eq = classOf[Equals]
      val serial = classOf[Serializable]
      val obj = classOf[Object]
      val prod = classOf[Product]
      val test = !c.getPackageName.startsWith("truediff") && !c.equals(eq) && !c.equals(serial) && !c.equals(prod)  && !c.equals(obj) && !c.equals(clazz)
      test
    }
  }

  private def allSuperTypesOf(clazz: Class[_]): List[Class[_]] = {
    if (clazz == null)
      return Nil
    val superinterfaces = clazz.getInterfaces.toList
    clazz::allSuperTypesOf(clazz.getSuperclass)++superinterfaces.flatMap(allSuperTypesOf)
  }

}
