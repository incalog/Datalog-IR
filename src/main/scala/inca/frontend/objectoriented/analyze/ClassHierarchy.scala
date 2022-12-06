package inca.frontend.objectoriented.analyze

import inca.backend.analyze.Graph
import inca.frontend.objectoriented.analyze.ClassHierarchy._
import inca.frontend.objectoriented.core._

object ClassHierarchy {
  sealed trait DependencyEdge {
    val color: String = "black"
    val label: String = ""
  }
  case object InheritanceEdge extends DependencyEdge

  sealed trait NodeType {
    val shape: String = "circle"
    val fillColor: String = "white"
    val fontColor: String = "black"
  }
  case class ClassNode(classDef: ClassDef) extends NodeType {
    val name: String = classDef.name.raw
    override def toString: String = name
  }
}

class ClassHierarchy(module: Module) extends Graph[ClassNode, DependencyEdge] {

  private val clsMap: Map[Name, ClassDef] = module.classes.map(c => c.name -> c).toMap

  module.classes.foreach { cls =>
    addNode(ClassNode(cls))
    cls.parentClassRefs.map { p =>
      addEdge(ClassNode(cls), ClassNode(clsMap(p.name)), InheritanceEdge)
    }
  }

  override protected def nodeToGraphViz(n: ClassNode): String = n.name.replace("$", "_")

  override protected def nodeGraphVizAttributes(n: ClassNode): String = Map(
      "shape" -> n.shape,
      "label" -> n.name,
      "fontcolor" -> n.fontColor,
      "fillcolor" -> n.fillColor,
      "style" -> "filled"
    ).map { case (k, v) =>
      s"""$k="$v""""
    }.mkString(", ")

  override protected def edgeGraphVizAttributes(from: ClassNode, to: ClassNode, kind: DependencyEdge): String = Map(
      "color" -> kind.color,
      "label" -> s""""${kind.label}""""
    ).map { case (k, v) =>
      s"$k=$v"
    }.mkString(", ")
}
