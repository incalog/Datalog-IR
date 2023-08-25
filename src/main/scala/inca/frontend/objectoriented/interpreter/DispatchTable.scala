package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.core._

case class DispatchTable(classes: Seq[ClassDef]) {
  private val table = generateDispatchTable(classes)

  def lookup(className: Name, methodName: Name): MethodDef = table.get((className, methodName)) match {
    case Some(value) => value
    case _ => throw new IllegalStateException(s"Could not lookup method $className.$methodName")
  }

  private def generateDispatchTable(classes: Seq[ClassDef]): Map[(Name, Name), MethodDef] = {
    def collectMethods(classDef: ClassDef)(implClass: ClassDef = classDef): Map[(String, ClassDef), (ClassDef, MethodDef)] = {
      val methods = implClass.content.flatMap {
        case m: MethodDef if !m.isStatic || (classDef == implClass) => Some((m.name.raw, classDef) -> (implClass, m))
        case _ => None
      }.toMap

      val parentMethods = implClass.parentClassRefs.flatMap { ref =>
        ref.target.getOrElse(throw new IllegalArgumentException(s"Unresolved class ${ref.name.raw}")) match {
          case parentClassDef: ClassDef => collectMethods(classDef)(parentClassDef)
          case _ => throw new IllegalStateException("Expected class ref, but found generic type")
        }

      }.toMap
      // We rely on the default map collision behaviour to find the concrete implementation class
      parentMethods ++ methods
    }

    val dispatchRules: Map[String, Seq[(ClassDef, (ClassDef, MethodDef))]] = classes
      .flatMap(c => collectMethods(c)())
      .groupBy(_._1._1).view.mapValues(_.map(v => v._1._2 -> v._2)).toMap
    dispatchRules.flatMap {
      case (_, classMapping) =>
        classMapping.map { case (c, (implC, method)) =>
          (c.name, method.name) -> method
        }
    }
  }
}
